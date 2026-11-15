%%%-------------------------------------------------------------------
%%% Copyright (C) 2020, Niclas Axelsson
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
-module(nova_plugin).
-moduledoc """
Plugins run at the beginning or at the end of a request. They can modify both the actual request or the nova-state. A plugin is implemented with the nova_plugin behaviour and needs to implement pre_security/4, pre_request/4 and/or post_request/4, and plugin_info/0.

A plugin returns one of:

`{ok, Req, State}` continues with the next plugin.

`{ok, Reply, Req, State}` sets the reply on `Req` and continues. `Reply` must be a `{reply, ...}` here: the controller answers later, so a redirect or a bare status would be overwritten. Any other reply renders the 500 status page and logs that `stop` is needed.

`{stop, Req, State}` stops the execution. The plugin is responsible in this case for returning a proper response to the client. `{stop, Reply, Req, State}` stops and sends `Reply`. `{stop, Req}` is a deprecated form of `{stop, Req, State}` that keeps the plugin's state as it was; it will be removed in 1.0.

`{break, Req, State}` and `{break, Reply, Req, State}` are deprecated aliases of the matching `stop` returns and will be removed in 1.0. They log a warning, once per plugin module; return `stop` instead.

`{error, Reason}` stops the execution and renders the 500 status page.

`Reply` is `{reply, Body}`, `{reply, Status, Body}`, `{reply, Status, Headers, Body}`, `{redirect, Url}`, `{redirect_local, Path}`, `{status, Status}` or `{status, Status, Headers}`. `Headers` is a map or a list of `{Name, Value}` pairs. Any other reply renders the 500 status page.

When a pre_request plugin stops the request, or the security function refuses it, the controller and the post_request plugins do not run.

## Before and after the security callback

A plugin listed as `{pre_request, Module, Options}` takes part twice before the controller, with the same options:

- `pre_security/4`, when the module has it, runs ahead of the security callback of the route, for every request to the route. It is for what has to happen whether or not the request is let in: answering a CORS preflight, which carries no credentials, giving the request its correlation id, turning a client away by address or rate. It must not read the request body. It is also all that runs ahead of the `405` for a CORS preflight to a route that does not take `OPTIONS`, of the plugins of the route for the method the preflight asks about.
- `pre_request/4` runs once the security callback has let the request through, and at once on a route without one. Reading and decoding the body belongs here, so that nothing is read from a client that is refused.

Earlier releases ran `pre_request/4` ahead of the security callback. Two lists, each in a route group or in the `nova` environment (the route group wins), change the order for the modules they name:

- `plugins_before_security` - `pre_request/4` of these modules runs ahead of the security callback, as it did. `all` names every plugin of the route group. A list has to name plugins that have `pre_request/4`, and the list of a route group plugins that the group lists as `pre_request`: any other name stops the boot with `{invalid_setting, plugins_before_security, List}`.
- `plugins_after_security` - nothing of these modules runs ahead of the security callback: `pre_security/4` runs behind it too, right before `pre_request/4`. A module in both lists runs after.

Within each of the two phases the plugins run in the order they are listed. What the security callback finds in the request is what `pre_security/4` put there. With the plugins of Nova that is the correlation id, the CORS headers and `parsed_qs`, and not `json`, `params`, `body` or `files`: a security callback that needs the decoded body lists `nova_request_plugin` under `plugins_before_security` for its route group.

Two things that this order changes without anything failing are warned about, once for a route group, when its routes are compiled:

- `nova_request_plugin` decodes the body behind the security callback of the route group, which then finds no `json` or `params` in the request.
- A plugin from outside Nova only has `pre_request/4` and is not listed under `plugins_before_security`, in a route group that has a security callback or a route that does not take `OPTIONS`. It does not run for a request that is refused, nor for a CORS preflight to such a route. A plugin that has `pre_security/4` is not named.

The state a plugin returns is kept per plugin module and shared by every request. Nova writes it back only when it changed, and concurrent requests overwrite each other's writes (the last writer wins). Keep per-request data in `Req`, and use `ets` or `counters` for anything shared that has to add up, such as counters.

These callbacks run for HTTP requests. Websocket handlers run their plugins through `nova_ws_handler`, which has its own contract; see `nova_websocket`.

""".

-doc "The two points of a request a plugin can run at.".
-type request_type() :: pre_request | post_request.
-export_type([request_type/0, reply/0]).

-doc """
A reply a plugin hands to Nova to send. `{reply, ...}` takes a body, with a
status of `200` unless one is given. `{redirect, Url}` answers `302` with a
`location` header. `{redirect_local, Path}` does the same for a path on the
site the request was made to, and answers `400` for anything else.
`{status, ...}` answers with that status and no body.
""".
-type reply() :: {reply, Body :: iodata()} |
                 {reply, Status :: integer(), Body :: iodata()} |
                 {reply, Status :: integer(), Headers :: cowboy:http_headers() | [{binary(), iodata()}],
                  Body :: iodata()} |
                 {redirect, Url :: binary() | string()} |
                 {redirect_local, Path :: binary() | string()} |
                 {status, Status :: integer()} |
                 {status, Status :: integer(), Headers :: cowboy:http_headers() | [{binary(), iodata()}]}.

-doc """
Called when the plugin is first found in a routing table: at start for the
routes compiled then, and later for a plugin that first appears in routes
added at runtime. Returns the state of the plugin, any term, which
`pre_request/4` and `post_request/4` get as `PluginState`. A plugin without
`init/0` starts with `#{}`.
""".
-callback init() -> State :: nova:state().
-optional_callbacks([init/0]).

-doc """
Called with the state of the plugin when the plugin manager terminates,
which it does when `nova` stops. The return value is not used, and an
exception is logged.
""".
-callback stop(State :: nova:state()) -> ok.
-optional_callbacks([stop/1]).

-doc """
Called for every HTTP request on a route that lists the plugin under
`pre_request`, ahead of the security callback of the route. `Env` is the
Cowboy middleware environment, `Options` the options the plugin is configured
with and `PluginState` the state of the plugin. It can modify the request and
the state, and returns what `c:pre_request/4` can. It must not read the
request body.
""".
-callback pre_security(Req :: cowboy_req:req(), Env :: any(),
                       Options :: map(), PluginState :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {ok, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {error, Reason :: term()}.
-optional_callbacks([pre_security/4]).

-doc """
Called for every HTTP request on a route that lists the plugin under
`pre_request`, before the controller, once the security callback of the
route has let the request through. The arguments are those of
`c:pre_security/4`. See the module documentation for the returns and what
Nova does with each.
""".
-callback pre_request(Req :: cowboy_req:req(), Env :: any(),
                      Options :: map(), PluginState :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {ok, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req()} |
    {stop, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {error, Reason :: term()}.
-optional_callbacks([pre_request/4]).

-doc """
Called for every HTTP request on a route that lists the plugin under
`post_request`, after the controller has returned. The arguments and the
returns are those of `pre_request/4`.
""".
-callback post_request(Req :: cowboy_req:req(), Env :: any(),
                       Options :: map(), PluginState :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {ok, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req()} |
    {stop, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {error, Reason :: term()}.
-optional_callbacks([post_request/4]).

-doc """
Called when the plugin is added, and has to return a map with at least
`title` and `version`. The other keys describe the plugin and are not read by
Nova.
""".
-callback plugin_info() -> #{title := binary(),
                            version := binary(),
                            url := binary(),
                            authors := [binary()],
                            description := binary(),
                            requires => [PluginName :: binary() |
                                                       {PluginName :: binary(), Version :: binary()}],
                             options => [{Key :: atom(), OptionDescription :: binary()}]}.
