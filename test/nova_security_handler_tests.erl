-module(nova_security_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%% Logger handler callback: forwards every event to the test process.
-export([log/2]).

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 50 ->
            []
    end.

-define(SETUP, fun setup/0).
-define(CLEANUP, fun cleanup/1).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = nova_routing_trie:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_use_stacktrace, false),
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, reply, fun nova_test_helper:reply/2),
    meck:expect(cowboy_req, set_resp_headers, fun(Headers, Req) ->
        ExistingHeaders = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => maps:merge(ExistingHeaders, Headers)}
    end),
    meck:expect(cowboy_req, set_resp_body, fun(Body, Req) ->
        Req#{resp_body => Body}
    end),
    meck:expect(cowboy_req, set_resp_header, fun(Name, Value, Req) ->
        Headers = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => Headers#{Name => Value}}
    end),
    Prev.

cleanup(Prev) ->
    meck:unload(cowboy_req),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

req() ->
    nova_test_helper:mock_req(<<"GET">>, <<"/">>).

env() ->
    #{secure => false, app => test_app, controller_data => #{}}.

%%====================================================================
%% secure=false — passthrough
%%====================================================================

secure_false_passthrough_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Env = env(),
        ?assertEqual({ok, Req, Env}, nova_security_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Callback returns true
%%====================================================================

callback_true_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> true end,
        Env = (env())#{secure => Callback},
        ?assertEqual({ok, Req, Env}, nova_security_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Callback returns {true, AuthData}
%%====================================================================

callback_true_with_auth_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        AuthData = #{user_id => 42},
        Callback = fun(_) -> {true, AuthData} end,
        Env = (env())#{secure => Callback},
        {ok, Req1, _Env1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(AuthData, maps:get(auth_data, Req1))
    end}.

%%====================================================================
%% Callback returns {true, AuthData} for websocket handler
%%====================================================================

callback_true_auth_data_websocket_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        AuthData = #{user_id => 99},
        Callback = fun(_) -> {true, AuthData} end,
        Env = (env())#{secure => Callback,
                     cowboy_handler => nova_ws_handler,
                     arguments => #{existing => data}},
        %% nova_ws_handler takes it from the request, as a controller does.
        ?assertEqual({ok, Req#{auth_data => AuthData}, Env}, nova_security_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Callback returns {false, Headers} -> 401 stop
%%====================================================================

callback_false_headers_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"x-error">> => <<"denied">>},
        Callback = fun(_) -> {false, RespHeaders} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(401, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"denied">>, maps:get(<<"x-error">>, maps:get(resp_headers, Req1)))
    end}.

%%====================================================================
%% Callback returns {false, 403, Headers} -> 403 stop
%%====================================================================

callback_false_custom_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"x-reason">> => <<"forbidden">>},
        Callback = fun(_) -> {false, 403, RespHeaders} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"forbidden">>, maps:get(<<"x-reason">>, maps:get(resp_headers, Req1)))
    end}.

%%====================================================================
%% Callback returns {false, 403, Headers, Body} -> stop with body
%%====================================================================

callback_false_with_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"content-type">> => <<"text/plain">>},
        Body = <<"Access denied">>,
        Callback = fun(_) -> {false, 403, RespHeaders, Body} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(Body, maps:get(resp_body, Req1))
    end}.

%%====================================================================
%% Callback returns {redirect, "/login"} -> 302 stop
%%====================================================================

callback_redirect_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> {redirect, "/login"} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(302, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"/login">>, maps:get(<<"location">>, maps:get(resp_headers, Req1)))
    end}.

%% The routing guide documents the redirect target as a binary.
callback_redirect_binary_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_) -> {redirect, ~"/login"} end,
        {stop, Req1} = nova_security_handler:execute(req(), (env())#{secure => Callback}),
        ?assertEqual(302, maps:get(resp_status_code, Req1)),
        ?assertEqual(~"/login", maps:get(~"location", maps:get(resp_headers, Req1)))
    end}.

callback_redirect_with_control_bytes_is_a_401_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
        try
            [begin
                 Callback = fun(_) -> {redirect, Url} end,
                 {stop, Req1} = nova_security_handler:execute(req(), (env())#{secure => Callback}),
                 ?assertEqual(401, maps:get(resp_status_code, Req1)),
                 ?assertNot(maps:is_key(~"location", maps:get(resp_headers, Req1, #{}))),
                 ?assertMatch([{report, #{msg := ~"Security callback redirected to an invalid location"}} | _],
                              logged())
             end || Url <- [<<"/a", 0, "b">>, ~"/a\r\nset-cookie: x=1", <<"/x", 255>>]]
        after
            logger:remove_handler(?MODULE)
        end
    end}.

callback_redirect_invalid_location_is_a_401_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_) -> {redirect, [16#D800]} end,
        {stop, Req1} = nova_security_handler:execute(req(), (env())#{secure => Callback}),
        ?assertEqual(401, maps:get(resp_status_code, Req1)),
        ?assertNot(maps:is_key(~"location", maps:get(resp_headers, Req1, #{})))
    end}.

%%====================================================================
%% Callback returns unexpected value -> 401 stop
%%====================================================================

callback_unexpected_value_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> something_weird end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(401, maps:get(resp_status_code, Req1))
    end}.

%%====================================================================
%% A value that is not a callback -> 401 stop
%%====================================================================

invalid_secure_value_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [fun() ->
              {stop, Req1} = nova_security_handler:execute(req(), (env())#{secure => Invalid}),
              ?assertEqual(401, maps:get(resp_status_code, Req1))
      end || Invalid <- [true, enabled, {my_auth, check}, fun() -> true end]]}.

%%====================================================================
%% Callback crashes -> the 500 page in place of the route (stacktrace disabled)
%%====================================================================

callback_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> error(boom) end,
        Route = fun ?MODULE:module_info/1,
        Env = (env())#{secure => Callback, callback => Route},
        {ok, Req1, Env1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertNot(is_map_key(has_sent_resp, Req1)),
        ?assertEqual(false, maps:get(secure, Env1)),
        ?assertNotEqual(Route, maps:get(callback, Env1))
    end}.

%%====================================================================
%% Callback crashes with stacktrace enabled -> the 500 page as well
%%====================================================================

callback_crash_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        Req = req(),
        Callback = fun(_) -> error(boom) end,
        Env = (env())#{secure => Callback},
        {ok, Req1, _Env1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assert(is_map(maps:get(crash_info, Req1))),
        CrashInfo = maps:get(crash_info, Req1),
        ?assert(is_list(maps:get(stacktrace, CrashInfo)))
    end}.

%%====================================================================
%% Several callbacks on one route -> each has to allow, in order
%%====================================================================

%% A callback that tells the test it ran and then answers with Result.
answering(Name, Result) ->
    fun(_Req) ->
            self() ! {called, Name},
            case Result of
                raise -> error({raised_by, Name});
                _ -> Result
            end
    end.

called() ->
    receive
        {called, Name} -> [Name | called()]
    after 0 ->
        []
    end.

execute_all(Callbacks) ->
    execute_all(Callbacks, env()).

execute_all(Callbacks, Env) ->
    Result = nova_security_handler:execute(req(), Env#{secure => Callbacks}),
    {Result, called()}.

several_callbacks_allow_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"every callback runs, in order, and the request goes on as it came",
       fun() ->
               Callbacks = [answering(first, true), answering(second, true), answering(third, true)],
               ?assertEqual({{ok, req(), (env())#{secure => Callbacks}}, [first, second, third]},
                            execute_all(Callbacks))
       end},
      {"a list of one callback is that callback",
       fun() ->
               Callbacks = [answering(only, {true, only_data})],
               {{ok, Req, Env}, Called} = execute_all(Callbacks),
               ?assertEqual({only_data, Callbacks, [only]}, {maps:get(auth_data, Req), maps:get(secure, Env), Called})
       end},
      {"the data of the last callback that returns some is what the controller gets",
       [fun() ->
                {{ok, Req, _Env}, _Called} = execute_all([answering(first, First), answering(second, Second)]),
                ?assertEqual(Expected, maps:get(auth_data, Req, none))
        end || {First, Second, Expected} <- [{{true, first_data}, {true, second_data}, second_data},
                                             {{true, first_data}, true, first_data},
                                             {true, {true, second_data}, second_data},
                                             {true, true, none}]]},
      {"false among them is no callback, as it is on its own",
       ?_assertMatch({{ok, _Req, _Env}, [second]}, execute_all([false, answering(second, true)]))},
      {"a later callback finds the data of an earlier one in the request",
       fun() ->
               Second = fun(Req) -> {true, {seen, maps:get(auth_data, Req)}} end,
               {{ok, Req, _Env}, _Called} = execute_all([answering(first, {true, first_data}), Second]),
               ?assertEqual({seen, first_data}, maps:get(auth_data, Req))
       end},
      {"the data of a websocket route follows the same rule, and is in the request alone",
       [fun() ->
                Env = (env())#{cowboy_handler => nova_ws_handler, arguments => #{module => a_controller}},
                {{ok, Req, Env1}, _Called} = execute_all([answering(first, First), answering(second, Second)], Env),
                ?assertEqual({Expected, #{module => a_controller}},
                             {maps:get(auth_data, Req, none), maps:get(arguments, Env1)})
        end || {First, Second, Expected} <- [{{true, first_data}, {true, second_data}, second_data},
                                             {{true, first_data}, true, first_data},
                                             {true, true, none}]]}]}.

several_callbacks_refuse_test_() ->
    Refused = fun(Callbacks) ->
                      {{stop, Req}, Called} = execute_all(Callbacks),
                      {maps:get(resp_status_code, Req), maps:get(resp_headers, Req), Called}
              end,
    Crashed = fun(Callbacks) ->
                      {{ok, Req, Env}, Called} = execute_all(Callbacks),
                      {maps:get(resp_status_code, Req), maps:get(callback, Env), Called}
              end,
    Allowing = [true, {true, some_data}],
    {setup, fun setup/0, fun cleanup/1,
     [{"the first refusal is the response, and no later callback runs",
       [?_assertMatch({401, _Headers, [first]},
                      Refused([answering(first, false), answering(second, true)])),
        ?_assertMatch({401, #{~"x-by" := ~"first"}, [first]},
                      Refused([answering(first, {false, #{~"x-by" => ~"first"}}),
                               answering(second, {false, 403, #{~"x-by" => ~"second"}})])),
        ?_assertMatch({403, #{~"x-by" := ~"first"}, [first]},
                      Refused([answering(first, {false, 403, #{~"x-by" => ~"first"}}), answering(second, true)])),
        ?_assertMatch({403, #{~"x-by" := ~"first"}, [first]},
                      Refused([answering(first, {false, 403, #{~"x-by" => ~"first"}, ~"no"}),
                               answering(second, true)])),
        ?_assertMatch({302, #{~"location" := ~"/login"}, [first]},
                      Refused([answering(first, {redirect, "/login"}), answering(second, true)])),
        ?_assertMatch({401, _Headers, [first]},
                      Refused([answering(first, {ok, allowed}), answering(second, true)]))]},
      {"a refusal by a later callback is the response, whatever the earlier ones allowed",
       [[?_assertMatch({401, _Headers, [first, second]},
                       Refused([answering(first, Allow), answering(second, false)])),
         ?_assertMatch({403, #{~"x-by" := ~"second"}, [first, second]},
                       Refused([answering(first, Allow), answering(second, {false, 403, #{~"x-by" => ~"second"}})])),
         ?_assertMatch({302, #{~"location" := ~"/login"}, [first, second]},
                       Refused([answering(first, Allow), answering(second, {redirect, "/login"})])),
         ?_assertMatch({401, _Headers, [first, second, third]},
                       Refused([answering(first, Allow), answering(second, Allow), answering(third, false)]))]
        || Allow <- Allowing]},
      {"a callback that crashes ends the checks, and the 500 page takes the place of the route",
       [?_assertEqual({500, fun nova_error_controller:status_code/1, Called}, Crashed(Callbacks))
        || {Callbacks, Called} <- [{[answering(first, raise), answering(second, true)], [first]},
                                   {[answering(first, true), answering(second, raise)], [first, second]},
                                   {[answering(first, {true, some_data}), answering(second, raise),
                                     answering(third, false)], [first, second]}]]},
      {"a value among them that is not a callback refuses",
       [?_assertMatch({401, _Headers, [first]}, Refused([answering(first, true), true])),
        ?_assertMatch({401, _Headers, []}, Refused([enabled, answering(second, true)])),
        ?_assertMatch({401, _Headers, [second]}, Refused([false, answering(second, false)])),
        ?_assertMatch({401, _Headers, []}, Refused([]))]}]}.
