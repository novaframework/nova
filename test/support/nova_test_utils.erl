-module(nova_test_utils).

%% Test utility functions for Nova framework tests
-export([
         mock_cowboy_req/0,
         mock_cowboy_req/1,
         mock_env/0,
         mock_env/1,
         start_test_plugin_manager/0,
         stop_test_plugin_manager/0,
         cleanup_ets_tables/0,
         with_test_setup/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% @doc Create a minimal cowboy_req map for testing
%% @end
%%------------------------------------------------------------------------------
-spec mock_cowboy_req() -> cowboy_req:req().
mock_cowboy_req() ->
    mock_cowboy_req(#{}).

%%------------------------------------------------------------------------------
%% @doc Create a cowboy_req map with custom overrides
%% @end
%%------------------------------------------------------------------------------
-spec mock_cowboy_req(map()) -> cowboy_req:req().
mock_cowboy_req(Overrides) ->
    Defaults = #{
        method => <<"GET">>,
        path => <<"/">>,
        qs => <<>>,
        version => 'HTTP/1.1',
        headers => #{},
        peer => {{127,0,0,1}, 12345},
        host => <<"localhost">>,
        port => 8080,
        scheme => <<"http">>,
        has_body => false,
        body_length => 0
    },
    maps:merge(Defaults, Overrides).

%%------------------------------------------------------------------------------
%% @doc Create a minimal middleware environment map for testing
%% @end
%%------------------------------------------------------------------------------
-spec mock_env() -> cowboy_middleware:env().
mock_env() ->
    mock_env(#{}).

%%------------------------------------------------------------------------------
%% @doc Create a middleware environment map with custom overrides
%% @end
%%------------------------------------------------------------------------------
-spec mock_env(map()) -> cowboy_middleware:env().
mock_env(Overrides) ->
    Defaults = #{
        dispatch => [],
        middlewares => []
    },
    maps:merge(Defaults, Overrides).

%%------------------------------------------------------------------------------
%% @doc Start the plugin manager for tests
%% @end
%%------------------------------------------------------------------------------
-spec start_test_plugin_manager() -> ok.
start_test_plugin_manager() ->
    case whereis(nova_plugin_manager) of
        undefined ->
            {ok, _Pid} = nova_plugin_manager:start_link(),
            ok;
        _Pid ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Stop the plugin manager
%% @end
%%------------------------------------------------------------------------------
-spec stop_test_plugin_manager() -> ok.
stop_test_plugin_manager() ->
    case whereis(nova_plugin_manager) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, normal),
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Clean up ETS tables created during tests
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_ets_tables() -> ok.
cleanup_ets_tables() ->
    Tables = [nova_handlers, nova_router_table, nova_session_ets],
    [catch ets:delete(Table) || Table <- Tables, ets:info(Table) =/= undefined],
    ok.

%%------------------------------------------------------------------------------
%% @doc Run a test function with standard setup and teardown
%% Usage: with_test_setup(fun() -> ?assertEqual(1, 1) end)
%% @end
%%------------------------------------------------------------------------------
-spec with_test_setup(fun(() -> any())) -> any().
with_test_setup(TestFun) ->
    cleanup_ets_tables(),
    try
        TestFun()
    after
        cleanup_ets_tables()
    end.
