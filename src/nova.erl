%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Interface module for nova
%%% @end

-module(nova).

-export([
         get_main_app/0,
         application_loaded/1,
         get_env/2,
         set_env/2
        ]).

-type state() :: any().
-export_type([state/0]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the name of the main bw-application (The one that started
%% everything)
%%
%% @end
%%--------------------------------------------------------------------
-spec get_main_app() -> {ok, Application :: atom()} | undefined.
get_main_app() ->
    application:get_env(nova, bootstrap_application).

%%--------------------------------------------------------------------
%% @doc
%% Checks if a nova application is loaded
%%
%% @end
%%--------------------------------------------------------------------
-spec application_loaded(Application::atom()) -> boolean().
application_loaded(Application) ->
    %% First we need to check which application that is the main one
    {ok, MainApp} = get_main_app(),
    lists:any(fun({CompApp, _, _}) -> CompApp == Application end,
              application:get_env(MainApp, nova_applications, [])).


%%--------------------------------------------------------------------
%% @doc
%% Works as the regular application:get_env/3 but instead of giving
%% a specific application we target the application that started
%% nova.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_env(Parameter :: atom(), Default :: any()) -> term() | undefined.
get_env(Parameter, Default) ->
    case get_main_app() of
        {ok, App} ->
            application:get_env(App, Parameter, Default);
        _ ->
            Default
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sets an environment variable for the main application.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_env(Key :: atom(), Value :: any()) -> ok | {error, main_app_not_found}.
set_env(Key, Value) ->
    case get_main_app() of
        {ok, App} ->
            application:set_env(App, Key, Value);
        _ ->
            {error, main_app_not_found}
    end.
