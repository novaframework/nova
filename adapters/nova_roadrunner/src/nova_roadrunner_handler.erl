-module(nova_roadrunner_handler).
-moduledoc false.

%% The Roadrunner handler behind every Nova listener. It answers each
%% request with a `{send, [], Fun}` response, and the fun runs Nova's
%% pipeline on the request as `nova_req` describes it, so Nova sends the
%% response through `nova_roadrunner` when it has decided on one.

-behaviour(roadrunner_handler).

-export([handle/1]).
-export([run/1, request/2]).

-spec handle(roadrunner_req:request()) -> roadrunner_handler:result().
handle(Req) ->
    {{send, [], fun ?MODULE:run/1}, Req}.

%% nova_pipeline answers a request no step answered, so a final response
%% went out by the time it returns.
-spec run(roadrunner_req:request()) -> roadrunner_req:request().
run(RR) ->
    {Env, Compress} = listener_state(roadrunner_req:state(RR)),
    Metadata = logger:get_process_metadata(),
    ok = nova_roadrunner:start_request(),
    try
        final_req(nova_pipeline:execute(request(RR, Compress), Env))
    catch
        %% Roadrunner logs what escapes and answers 500. What is logged
        %% may hold the request, so it leaves as Nova logs a crash.
        Class:Reason:Stacktrace ->
            erlang:raise(Class, nova_redact:term(Reason), nova_redact:stacktrace(Stacktrace))
    after
        ok = nova_roadrunner:end_request(),
        ok = restore_metadata(Metadata)
    end.

final_req({ok, #{roadrunner_req := Last}, _Env}) -> Last;
final_req({stop, #{roadrunner_req := Last}}) -> Last.

%% The requests of a connection share its process. Roadrunner replaces the
%% logger metadata as a request starts, and what the request added, a
%% correlation id or a user, is taken off again as it ends, so it labels
%% nothing the connection logs afterwards.
restore_metadata(undefined) -> logger:unset_process_metadata();
restore_metadata(Metadata) -> logger:set_process_metadata(Metadata).

%% What nova_roadrunner:start_listener/2 gave the listener.
-spec listener_state(term()) -> {nova_server:env(), boolean()}.
listener_state(#{env := Env, compress := Compress}) when is_map(Env), is_boolean(Compress) ->
    {Env, Compress}.

-spec request(roadrunner_req:request(), boolean()) -> nova_req:req().
request(RR, Compress) ->
    Headers = headers(roadrunner_req:headers(RR), #{}),
    Scheme = roadrunner_req:scheme(RR),
    {Host, Port} = authority(Headers, Scheme),
    HasBody = has_body(RR, Headers),
    #{nova_server => nova_roadrunner,
      roadrunner_req => RR,
      roadrunner_compress => Compress,
      method => roadrunner_req:method(RR),
      version => version(roadrunner_req:version(RR)),
      scheme => atom_to_binary(Scheme),
      host => Host,
      port => Port,
      path => roadrunner_req:path(RR),
      qs => roadrunner_req:qs(RR),
      headers => Headers,
      peer => peer(roadrunner_req:peer(RR)),
      has_body => HasBody,
      body_length => body_length(Headers, HasBody)}.

%% A header sent more than once is one header with its values joined with
%% `, `, and `cookie` with `; ` (RFC 9113, 8.2.3).
headers([], Headers) ->
    Headers;
headers([{Name, Value} | Rest], Headers) ->
    case Headers of
        #{Name := Earlier} ->
            headers(Rest, Headers#{Name := <<Earlier/binary, (separator(Name))/binary, Value/binary>>});
        #{} ->
            headers(Rest, Headers#{Name => Value})
    end.

separator(~"cookie") -> ~"; ";
separator(_Name) -> ~", ".

version({1, 0}) -> 'HTTP/1.0';
version({1, 1}) -> 'HTTP/1.1';
version({2, 0}) -> 'HTTP/2';
version({3, 0}) -> 'HTTP/3'.

%% The host and port the client asked for. Only an HTTP/1.0 request may name
%% no host. A `host` that does not parse is kept as it was sent.
authority(#{~"host" := Authority}, Scheme) ->
    try cow_http_hd:parse_host(Authority) of
        {Host, undefined} -> {Host, default_port(Scheme)};
        {Host, Port} -> {Host, Port}
    catch
        _Class:_Reason -> {Authority, default_port(Scheme)}
    end;
authority(_Headers, Scheme) ->
    {<<>>, default_port(Scheme)}.

default_port(http) -> 80;
default_port(https) -> 443.

%% An address the OS could not report, of a socket already closing.
peer(undefined) -> {{0, 0, 0, 0}, 0};
peer(Peer) -> Peer.

%% Whether a body follows. Roadrunner knows from how the request is framed,
%% which it keeps on the reader the body is read through.
has_body(_RR, #{~"content-length" := Length}) ->
    Length =/= ~"0";
has_body(_RR, #{~"transfer-encoding" := _}) ->
    true;
has_body(#{body_reader := #{framing := stream, done := Done}}, _Headers) ->
    not Done;
has_body(#{body_reader := _Reader}, _Headers) ->
    false;
has_body(RR, _Headers) ->
    roadrunner_req:has_body(RR).

body_length(#{~"content-length" := Length}, _HasBody) ->
    try
        binary_to_integer(Length)
    catch
        error:badarg -> undefined
    end;
body_length(_Headers, false) ->
    0;
body_length(_Headers, true) ->
    undefined.
