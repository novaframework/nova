%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Interface module for nova
%%% @end

-module(nova).

-export([
         get_main_app/0,
         get_apps/0,
         get_environment/0,
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
%% Returns a proplist with all nova applications and their prefix. This
%% function is useful when calulating dynamic routes.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_apps() -> [{App :: atom(), Prefix :: list()}].
get_apps() ->
    nova_router:compiled_apps().

%%--------------------------------------------------------------------
%% @doc
%% Returns which environment nova is started in. This fetches the
%% environment-variable in sys.config for Nova and returns the value found.
%% Defaults to: dev
%%
%% @end
%%--------------------------------------------------------------------
-spec get_environment() -> any().
get_environment() ->
    application:get_env(nova, environment, dev).

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
