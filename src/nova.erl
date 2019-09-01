%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Interface module for nova
%%% @end

-module(nova).

-export([
         get_main_app/0,
         application_loaded/1,
         load_routefile/2
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
    nova_router:get_main_app().

%%--------------------------------------------------------------------
%% @doc
%% Checks if a nova application is loaded
%%
%% @end
%%--------------------------------------------------------------------
-spec application_loaded(Application::atom()) -> boolean().
application_loaded(Application) ->
    %% First we need to check which application that is the main one
    {ok, MainApp} = application:get_application(nova_sup),
    lists:any(fun({CompApp, _, _}) -> CompApp == Application end,
              application:get_env(MainApp, nova_applications)).

%%--------------------------------------------------------------------
%% @doc
%% Loads a route-file for a certain application
%%
%% @end
%%--------------------------------------------------------------------
-spec load_routefile(Application :: atom(), RoutePath :: string()) -> ok.
load_routefile(Application, RoutePath) ->
    nova_router:process_routefile(Application, RoutePath).
