-module(nova_cowboy).
-moduledoc """
Run Nova on [Cowboy](https://github.com/ninenines/cowboy).

Add `nova_cowboy` to the dependencies and the `applications` of the
application, and Nova's listeners run on Cowboy.

A request is Cowboy's own request map with Nova's keys added, so code that
calls `cowboy_req` keeps working on this server. Code that calls `nova_req`
works on every server.
""".

-behaviour(nova_server).

-export([
         capabilities/0,
         reply/4,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         inform/3,
         read_body/2,
         upgrade_websocket/5,
         response_begun/1
        ]).

-define(RESPONSE_BEGUN_TIMEOUT, 5000).

-spec capabilities() -> nova_server:capabilities().
capabilities() ->
    #{protocols => [http1, http2],
      cleartext => [http1, http2],
      websocket => [http1, http2]}.

%%%===================================================================
%%% Requests
%%%===================================================================

-spec reply(nova_server:status(), nova_server:headers(), nova_server:body(), Req) -> Req
              when Req :: nova_req:req().
reply(Status, Headers, Body, Req) ->
    {HeaderMap, Req0} = cowboy_headers(Headers, Req),
    cowboy_req:reply(Status, HeaderMap, Body, Req0).

-spec stream_reply(nova_server:status(), nova_server:headers(), Req) -> Req
              when Req :: nova_req:req().
stream_reply(Status, Headers, Req) ->
    {HeaderMap, Req0} = cowboy_headers(Headers, Req),
    cowboy_req:stream_reply(Status, HeaderMap, Req0).

%% The request a controller streams with may be the one from before the
%% headers went out, which cowboy_req has no clause for.
-spec stream_body(iodata(), fin | nofin, nova_req:req()) -> ok.
stream_body(Data, IsFin, Req) ->
    cowboy_req:stream_body(Data, IsFin, Req#{has_sent_resp => headers}).

-spec stream_trailers(nova_server:headers(), nova_req:req()) -> ok.
stream_trailers(Trailers, Req) ->
    cowboy_req:stream_trailers(header_map(Trailers), Req#{has_sent_resp => headers}).

-spec inform(nova_server:status(), nova_server:headers(), nova_req:req()) -> ok.
inform(Status, Headers, Req) ->
    cowboy_req:inform(Status, header_map(Headers), Req).

%% Cowboy exits with a bare timeout when the body does not arrive.
-spec read_body(Req, nova_server:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
              when Req :: nova_req:req().
read_body(Req, Opts) ->
    try
        cowboy_req:read_body(Req, Opts)
    catch
        exit:timeout ->
            exit({request_error, timeout, 'The request body was not received within the configured time.'})
    end.

-spec upgrade_websocket(Req, Env, module(), term(), nova_server:ws_opts()) ->
          {ok, Req, Env} | {stop, Req} when Req :: nova_req:req(), Env :: nova_server:env().
upgrade_websocket(Req, Env, Handler, State, Opts) ->
    cowboy_websocket:upgrade(Req, Env, Handler, State, Opts).

%% A response sent with cowboy_req went by the stream handler in the
%% connection process, so that is where it is known.
-spec response_begun(nova_req:req()) -> nova_server:response_progress().
response_begun(#{nova_stream := true, pid := _, streamid := _} = Req) ->
    Ref = make_ref(),
    cowboy_req:cast({nova_response_begun, self(), Ref}, Req),
    receive
        {Ref, Sent} -> Sent
    after ?RESPONSE_BEGUN_TIMEOUT ->
            false
    end;
response_begun(_Req) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Nova merged the headers and cookies of the request into the list, so the
%% request Cowboy gets carries none, or Cowboy would add them again. Cowboy
%% takes cookies apart from the headers, as complete set-cookie values.
cowboy_headers(Headers, Req) ->
    {Cookies, Others} = lists:partition(fun({Name, _Value}) -> Name =:= ~"set-cookie" end, Headers),
    Req0 = maps:without([resp_headers, resp_cookies, resp_body], Req),
    case Cookies of
        [] ->
            {header_map(Others), Req0};
        _ ->
            Indexed = lists:zip(lists:seq(1, length(Cookies)), [Value || {_Name, Value} <- Cookies]),
            {header_map(Others), Req0#{resp_cookies => maps:from_list(Indexed)}}
    end.

%% A header named more than once is one header with its values joined.
header_map(Headers) ->
    header_map(Headers, #{}).

header_map([], Map) ->
    Map;
header_map([{Name, Value} | Rest], Map) ->
    case Map of
        #{Name := Earlier} -> header_map(Rest, Map#{Name := [Earlier, ~", ", Value]});
        #{} -> header_map(Rest, Map#{Name => Value})
    end.
