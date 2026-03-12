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
    %% PubSub bridge
    broadcast/2,
    broadcast_from/3,
    subscribe/1,
    subscribe/2,
    unsubscribe/1,
    unsubscribe/2
]).

%% Arizona modules are optional runtime dependencies
-ignore_xref([{arizona_cowboy_request, new, 1},
              {arizona_view, call_mount_callback, 3},
              {arizona_renderer, render_layout, 1},
              {arizona_pubsub, broadcast, 2},
              {arizona_pubsub, broadcast_from, 3},
              {arizona_pubsub, join, 2},
              {arizona_pubsub, leave, 2}]).
-dialyzer({nowarn_function, [render_view/3, broadcast/2, broadcast_from/3,
                             subscribe/1, subscribe/2, unsubscribe/1, unsubscribe/2]}).

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
%%   2. Auto-registers /live WebSocket endpoint for Arizona connections
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
%% Broadcast a message to all Arizona view subscribers of a topic.
%% Views receive this in their `handle_event(Topic, Data, View)' callback.
%%
%% Example from a Nova controller:
%% ```
%% nova_arizona:broadcast(<<"user_updated">>, #{id => UserId}).
%% '''
%% @end
%%--------------------------------------------------------------------
-spec broadcast(Topic :: binary(), Data :: term()) -> ok.
broadcast(Topic, Data) ->
    arizona_pubsub:broadcast(Topic, Data).

%%--------------------------------------------------------------------
%% @doc
%% Broadcast a message to all subscribers except the sender.
%% @end
%%--------------------------------------------------------------------
-spec broadcast_from(From :: pid(), Topic :: binary(), Data :: term()) -> ok.
broadcast_from(From, Topic, Data) ->
    arizona_pubsub:broadcast_from(From, Topic, Data).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe the calling process to an Arizona PubSub topic.
%% The process will receive `{pubsub_message, Topic, Data}' messages.
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Topic :: binary()) -> ok.
subscribe(Topic) ->
    arizona_pubsub:join(Topic, self()).

%% @doc Subscribe a specific process to an Arizona PubSub topic.
-spec subscribe(Topic :: binary(), Pid :: pid()) -> ok.
subscribe(Topic, Pid) ->
    arizona_pubsub:join(Topic, Pid).

%% @doc Unsubscribe the calling process from an Arizona PubSub topic.
-spec unsubscribe(Topic :: binary()) -> ok | not_joined.
unsubscribe(Topic) ->
    arizona_pubsub:leave(Topic, self()).

%% @doc Unsubscribe a specific process from an Arizona PubSub topic.
-spec unsubscribe(Topic :: binary(), Pid :: pid()) -> ok | not_joined.
unsubscribe(Topic, Pid) ->
    arizona_pubsub:leave(Topic, Pid).


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
        handler = arizona_websocket,
        arguments = #{},
        plugins = [],
        secure = false
    },
    ?LOG_DEBUG(#{action => <<"Adding Arizona WebSocket route">>,
                 route => LivePath}),
    routing_tree:insert('_', LivePath, '_', Value, Dispatch).
