-module(nova_sup_tests).
-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

session_manager_without_session_exists_is_warned_about_test_() ->
    {setup,
     fun() -> application:set_env(nova, use_sessions, true) end,
     fun(_) -> application:unset_env(nova, use_sessions) end,
     fun() ->
             ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
             try
                 nova_sup:warn_without_session_exists(nova_session_ets, nova_session_ets:module_info(exports)),
                 ?assertEqual([], logged()),
                 nova_sup:warn_without_session_exists(my_sessions, [{get_value, 2}]),
                 [{report, #{session_manager := my_sessions}}] = logged()
             after
                 logger:remove_handler(?MODULE)
             end
     end}.

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 100 ->
            []
    end.
