%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Interface module for nova
%%% @end

-module(nova).

-export([
         get_main_app/0,
         application_loaded/1,
         get_env/2
        ]).


%%--------------------------------------------------------------------
%% @doc
%% Returns the name of the main bw-application (The one that started
%% everything)
%%
%% @end
%%--------------------------------------------------------------------
-spec get_main_app() -> {ok, Application :: atom()}.
get_main_app() ->
    {ok, nova_router:get_main_app()}.

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
-spec get_env(Parameter :: atom(), Default :: any()) -> any().
get_env(Parameter, Default) ->
    application:get_env(nova_router:get_main_app(), Parameter, Default).
