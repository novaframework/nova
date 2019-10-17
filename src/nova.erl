%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Interface module for nova
%%% @end

-module(nova).

-export([
         get_main_app/0,
         application_loaded/1
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
    application:get_application(nova_sup).

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
