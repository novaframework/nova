%%%-------------------------------------------------------------------
%%% @doc
%%% Bridge module for Arizona LiveView integration with Nova.
%%%
%%% Provides rendering of Arizona views as Nova controller callbacks
%%% and manages the Arizona dispatch table for WebSocket view resolution.
%%%
%%% Arizona is an optional dependency - this module only has runtime
%%% dependencies on Arizona modules, which are only invoked when
%%% liveview routes are actually defined.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_arizona).

-export([
    render_view/3,
    register_view/3,
    finalize/1,
    resolve_view/1
]).

%% Arizona modules are optional runtime dependencies
-ignore_xref([{arizona_cowboy_request, new, 1},
              {arizona_view, call_mount_callback, 3},
              {arizona_renderer, render_layout, 1},
              {cowboy_router, execute, 2},
              resolve_view/1]).
-dialyzer({nowarn_function, [render_view/3, resolve_view/1, finalize/1]}).

-include("../include/nova_router.hrl").
-include_lib("kernel/include/logger.hrl").

-define(ARIZONA_VIEWS_KEY, nova_arizona_views).

%%--------------------------------------------------------------------
%% @doc
%% Called from nova_router during route compilation for each liveview
%% route. Accumulates view registrations in persistent_term.
%% @end
%%--------------------------------------------------------------------
-spec register_view(Path :: string(), ViewModule :: module(), MountArg :: term()) -> ok.
register_view(Path, ViewModule, MountArg) ->
    Views = persistent_term:get(?ARIZONA_VIEWS_KEY, []),
    persistent_term:put(?ARIZONA_VIEWS_KEY, [{Path, ViewModule, MountArg} | Views]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called from nova_router after all routes have been compiled.
%% If any liveview routes were registered:
%%   1. Builds Arizona's Cowboy dispatch table (persistent_term) so
%%      arizona_websocket can resolve views from ?path= query params
%%   2. Sets Arizona's pubsub scope to nova_scope
%%   3. Auto-registers /live WebSocket endpoint for Arizona connections
%%
%% Returns the dispatch tree unchanged if no liveview routes exist.
%% @end
%%--------------------------------------------------------------------
-spec finalize(Dispatch :: term()) -> term().
finalize(Dispatch) ->
    Views = persistent_term:get(?ARIZONA_VIEWS_KEY, []),
    case Views of
        [] ->
            Dispatch;
        _ ->
            setup_arizona_dispatch(Views),
            setup_live_websocket(Dispatch)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Nova controller callback for rendering Arizona views.
%% Returns {status, 200, Headers, Html} which Nova's handler system
%% processes via nova_basic_handler:handle_status/3.
%% @end
%%--------------------------------------------------------------------
-spec render_view(ViewModule :: module(), MountArg :: term(), Req :: cowboy_req:req()) ->
    {status, 200, map(), iodata()}.
render_view(ViewModule, MountArg, Req) ->
    ArizonaReq = arizona_cowboy_request:new(Req),
    View = arizona_view:call_mount_callback(ViewModule, MountArg, ArizonaReq),
    {Html, _RenderView} = arizona_renderer:render_layout(View),
    {status, 200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, Html}.

%%--------------------------------------------------------------------
%% @doc
%% Resolve a view module from the Arizona dispatch table.
%% Used by the WebSocket handler to determine which view to mount.
%% @end
%%--------------------------------------------------------------------
-spec resolve_view(Req :: cowboy_req:req()) -> {view, module(), term(), list()}.
resolve_view(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    maps:get(handler_opts, Env).


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Build Arizona's Cowboy dispatch table from accumulated view routes.
%% This allows arizona_websocket to resolve view modules when clients
%% connect to /live?path=/some-page via arizona_server:get_handler_opts/1.
setup_arizona_dispatch(Views) ->
    CowboyRoutes = [{list_to_binary(Path), arizona_view_handler,
                     {view, Mod, MountArg, []}}
                    || {Path, Mod, MountArg} <- Views],
    ArizonaDispatch = cowboy_router:compile([{'_', CowboyRoutes}]),
    persistent_term:put(arizona_dispatch, ArizonaDispatch),
    ?LOG_DEBUG(#{action => <<"Arizona dispatch table built">>,
                 view_count => length(Views)}).

%% Auto-register /live WebSocket endpoint for Arizona connections.
setup_live_websocket(Dispatch) ->
    LivePath = application:get_env(nova, arizona_live_path, "/live"),
    Value = #cowboy_handler_value{
        app = nova,
        handler = arizona_nova_websocket,
        arguments = #{view_resolver => fun nova_arizona:resolve_view/1},
        plugins = [],
        secure = false
    },
    ?LOG_DEBUG(#{action => <<"Adding Arizona WebSocket route">>,
                 route => LivePath}),
    routing_tree:insert('_', LivePath, '_', Value, Dispatch).
