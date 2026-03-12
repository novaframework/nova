%% @doc Rate limiting plugin using a sliding window counter in ETS.
%%
%% Limits requests per client IP (or custom key function) within a
%% configurable time window. Returns 429 Too Many Requests when exceeded.
%%
%% == Options ==
%% <ul>
%%   <li>`max_requests' — max requests per window (default 100)</li>
%%   <li>`window_ms' — window size in milliseconds (default 60000 = 1 min)</li>
%%   <li>`paths' — list of path prefixes to rate-limit (default all paths)</li>
%%   <li>`key_fun' — fun(Req) -> binary() for custom client keys</li>
%% </ul>
-module(nova_rate_limit_plugin).
-behaviour(nova_plugin).

-export([
         init/0,
         stop/1,
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-define(TABLE, nova_rate_limit_entries).

%%--------------------------------------------------------------------
%% init/0 — create ETS table for rate limit counters
%%--------------------------------------------------------------------
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [public, named_table, set, {write_concurrency, true}]);
        _ ->
            ok
    end,
    #{}.

%%--------------------------------------------------------------------
%% stop/1
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% pre_request — check and enforce rate limit
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req = #{path := Path}, _Env, Options, State) ->
    Paths = maps:get(paths, Options, []),
    case should_limit(Path, Paths) of
        true ->
            check_rate(Req, Options, State);
        false ->
            {ok, Req, State}
    end.

%%--------------------------------------------------------------------
%% post_request — pass-through
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% plugin_info
%%--------------------------------------------------------------------
plugin_info() ->
    #{title => <<"Nova Rate Limit Plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Sliding window rate limiting per client IP.">>,
      options => [
          {max_requests, <<"Max requests per window (default: 100)">>},
          {window_ms, <<"Window size in milliseconds (default: 60000)">>},
          {paths, <<"List of path prefixes to limit (default: all paths)">>},
          {key_fun, <<"fun(Req) -> binary() for custom client identification">>}
      ]}.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

should_limit(_Path, []) ->
    true;
should_limit(Path, Prefixes) ->
    lists:any(fun(Prefix) ->
        case binary:match(Path, Prefix) of
            {0, _} -> true;
            _ -> false
        end
    end, Prefixes).

check_rate(Req, Options, State) ->
    MaxRequests = maps:get(max_requests, Options, 100),
    WindowMs = maps:get(window_ms, Options, 60000),
    Key = client_key(Req, Options),
    Now = erlang:system_time(millisecond),
    WindowStart = Now - WindowMs,
    case ets:lookup(?TABLE, Key) of
        [{Key, Count, FirstRequestTime}] when FirstRequestTime >= WindowStart ->
            if
                Count >= MaxRequests ->
                    RetryAfter = integer_to_binary((FirstRequestTime + WindowMs - Now) div 1000 + 1),
                    {stop, {reply, 429,
                            [{<<"content-type">>, <<"text/plain">>},
                             {<<"retry-after">>, RetryAfter}],
                            <<"Too Many Requests">>}, Req, State};
                true ->
                    ets:update_counter(?TABLE, Key, {2, 1}),
                    {ok, Req, State}
            end;
        _ ->
            ets:insert(?TABLE, {Key, 1, Now}),
            {ok, Req, State}
    end.

client_key(Req, #{key_fun := KeyFun}) ->
    KeyFun(Req);
client_key(Req, _Options) ->
    peer_ip(Req).

peer_ip(#{peer := {IP, _Port}}) ->
    iolist_to_binary(inet:ntoa(IP));
peer_ip(_Req) ->
    <<"unknown">>.
