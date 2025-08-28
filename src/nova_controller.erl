%%% @doc
%%% Nova Controller behaviour definition
%%% @end

-module(nova_controller).

-export([routes/2]).

-optional_callbacks([routes/2]).

%% @doc Returns the routes defined in the module, if any
-callback routes(Env :: atom(), Opts :: map()) -> Routes :: [map()].


-spec routes(module() | {module(), map()}, Env :: atom()) -> Routes :: [map()].
routes({Module, Opts}, Env) when is_map(Opts) ->
    routes(Module, Env, Opts);
routes(Module, Env) ->
    routes(Module, Env, #{}).

routes(Module, Env, Opts) ->
    %% try to ensure callback module is loaded first
    try Module:module_info(module)
    catch _:_ -> ok
    end,
    case erlang:function_exported(Module, routes, 2) of
        true ->
            Module:routes(Env, Opts);
        false ->
            []
    end.
