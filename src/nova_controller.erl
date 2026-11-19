-module(nova_controller).
-moduledoc """
The behaviour of a controller that declares its own routes. A router lists
such controllers in `c:nova_router:controllers/1`. A controller that only
has its functions named in the routes of a router does not need it.
""".

-export([routes/2]).

-optional_callbacks([routes/2]).

-doc """
Called when the routes of the application are compiled, for a controller the
router lists in `c:nova_router:controllers/1`. `Env` is what
`nova:get_environment/0` returns. `Opts` is the map given with the module in
that list, or `#{}` for a module listed by name.

Returns route groups, each a map as a router returns them from
`c:nova_router:routes/1`.
""".
-callback routes(Env :: atom(), Opts :: map()) -> Routes :: [map()].


-doc """
The routes of a controller, given as `Module` or `{Module, Opts}`: what its
`c:routes/2` returns, or `[]` when the module does not export it.
""".
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
