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

- `pre_security/4`, when the module has it, runs ahead of the security callback of the route, for every request to the route. It is for what has to happen whether or not the request is let in: answering a CORS preflight, which carries no credentials, giving the request its correlation id, turning a client away by address or rate. It must not read the request body.
- `pre_request/4` runs once the security callback has let the request through, and at once on a route without one. Reading and decoding the body belongs here, so that nothing is read from a client that is refused.

Earlier releases ran `pre_request/4` ahead of the security callback. Two lists, each in a route group or in the `nova` environment (the route group wins), change the order for the modules they name:

- `plugins_before_security` - `pre_request/4` of these modules runs ahead of the security callback, as it did. `all` names every plugin of the route group. A list has to name plugins that have `pre_request/4`, and the list of a route group plugins that the group lists as `pre_request`: any other name stops the boot with `{invalid_setting, plugins_before_security, List}`.
- `plugins_after_security` - nothing of these modules runs ahead of the security callback: `pre_security/4` runs behind it too, right before `pre_request/4`. A module in both lists runs after.

Within each of the two phases the plugins run in the order they are listed. What the security callback finds in the request is what `pre_security/4` put there. With the plugins of Nova that is the correlation id, the CORS headers and `parsed_qs`, and not `json`, `params`, `body` or `files`: a security callback that needs the decoded body lists `nova_request_plugin` under `plugins_before_security` for its route group.

When a route has a security callback, a plugin from outside Nova that only has `pre_request/4` and is in neither list is named in a notice when the routes are compiled, once, since it no longer runs for requests that are refused.

The state a plugin returns is kept per plugin module and shared by every request. Nova writes it back only when it changed, and concurrent requests overwrite each other's writes (the last writer wins). Keep per-request data in `Req`, and use `ets` or `counters` for anything shared that has to add up, such as counters.

These callbacks run for HTTP requests. Websocket handlers run their plugins through `nova_ws_handler`, which has its own contract; see `nova_websocket`.

""".

-type request_type() :: pre_request | post_request.
-export_type([request_type/0, reply/0]).

-type reply() :: {reply, Body :: iodata()} |
                 {reply, Status :: integer(), Body :: iodata()} |
                 {reply, Status :: integer(), Headers :: cowboy:http_headers() | [{binary(), iodata()}],
                  Body :: iodata()} |
                 {redirect, Url :: binary() | string()} |
                 {redirect_local, Path :: binary() | string()} |
                 {status, Status :: integer()} |
                 {status, Status :: integer(), Headers :: cowboy:http_headers() | [{binary(), iodata()}]}.

-doc """
Start function for the plugin. This function is called when the plugin is started
and will return a state that will be passed to the other functions during
the life cycle of the plugin. The state can be any term.
""".
-callback init() -> State :: nova:state().
-optional_callbacks([init/0]).

-doc """
Stop function for the plugin. This function is called when the application is stopped.
It takes the plugin's state as argument and should return ok.
""".
-callback stop(State :: nova:state()) -> ok.
-optional_callbacks([stop/1]).

-doc """
This function is called ahead of the security callback of the route, for every
request to it. It can modify the request and the nova-state, and returns what
`c:pre_request/4` can. It must not read the request body.
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
This function is called before the controller, once the security callback of
the route has let the request through. It can modify the request and the
nova-state. See the module documentation for what it can return.
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
This function is called after the request is processed. It can modify the request.
See the module documentation for what it can return.
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
This function should return information about the plugin. The information is used
in the documentation and in the plugin-listing.
""".
-callback plugin_info() -> #{title := binary(),
                            version := binary(),
                            url := binary(),
                            authors := [binary()],
                            description := binary(),
                            requires => [PluginName :: binary() |
                                                       {PluginName :: binary(), Version :: binary()}],
                             options => [{Key :: atom(), OptionDescription :: binary()}]}.
