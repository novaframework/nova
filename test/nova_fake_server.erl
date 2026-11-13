-module(nova_fake_server).
-moduledoc false.

%% A nova_server for nova_req_tests: each request callback sends the request's
%% `owner' the call it got and returns what the request's `fake_results' map
%% holds for it, so the tests see what nova_req hands the server.

-behaviour(nova_server).

-export([
         start_listener/3,
         stop_listener/1,
         drain_listeners/2,
         reply/4,
         inform/3,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         read_body/2,
         read_part/2,
         read_part_body/2,
         upgrade/5,
         upgrade/6
        ]).

start_listener(_Ref, _Configuration, _Env) -> {error, not_a_listener}.

stop_listener(_Ref) -> ok.

drain_listeners(_Refs, _Timeout) -> ok.

reply(Status, Headers, Body, Req) ->
    record({reply, Status, Headers, Body}, Req),
    Req#{has_sent_resp => true}.

inform(Status, Headers, Req) ->
    record({inform, Status, Headers}, Req),
    ok.

stream_reply(Status, Headers, Req) ->
    record({stream_reply, Status, Headers}, Req),
    Req#{has_sent_resp => headers}.

stream_body(Data, IsFin, Req) ->
    record({stream_body, iolist_to_binary(Data), IsFin}, Req),
    ok.

stream_trailers(Trailers, Req) ->
    record({stream_trailers, Trailers}, Req),
    ok.

read_body(Req, Opts) ->
    record({read_body, Opts}, Req),
    result(read_body, Req).

read_part(Req, Opts) ->
    record({read_part, Opts}, Req),
    result(read_part, Req).

read_part_body(Req, Opts) ->
    record({read_part_body, Opts}, Req),
    result(read_part_body, Req).

upgrade(Mod, Req, Env, _Handler, _State) ->
    record({upgrade, Mod}, Req),
    {ok, Req, Env}.

upgrade(Mod, Req, Env, _Handler, _State, Opts) ->
    record({upgrade, Mod, Opts}, Req),
    {ok, Req, Env}.

record(Call, #{owner := Owner}) ->
    Owner ! {?MODULE, Call}.

result(Name, #{fake_results := Results} = Req) ->
    case maps:get(Name, Results) of
        {Tag, Data} -> {Tag, Data, Req};
        done -> {done, Req}
    end.
