%%%-------------------------------------------------------------------
%%% Copyright (C) 2020, Niclas Axelsson
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
-module(nova_plugin).
-moduledoc """
Plugins can be run at two different times; either in the beginning or at the end of a request. They can modify both the actual request or the nova-state. A plugin is implemented with the nova_plugin behaviour and needs to implement pre_request/4 and/or post_request/4 and plugin_info/0.

A plugin returns one of:

`{ok, Req, State}` continues with the next plugin.

`{ok, Reply, Req, State}` sets the reply on `Req` and continues.

`{stop, Req, State}` and `{stop, Req}` stop the execution. The plugin is responsible in this case for returning a proper response to the client. `{stop, Reply, Req, State}` stops and sends `Reply`.

`{break, Req, State}` and `{break, Reply, Req, State}` are deprecated aliases of the matching `stop` returns and will be removed in 1.0. They log a warning; return `stop` instead.

`{error, Reason}` stops the execution and renders the 500 status page.

`Reply` is `{reply, Body}`, `{reply, Status, Body}`, `{reply, Status, Headers, Body}`, `{redirect, Url}`, `{status, Status}` or `{status, Status, Headers}`. Any other reply renders the 500 status page.

When a pre_request plugin stops the request, or the security function refuses it, the controller and the post_request plugins do not run.

The state a plugin returns is kept per plugin module and shared by every request. Nova writes it back only when it changed, and concurrent requests overwrite each other's writes (the last writer wins). Keep per-request data in `Req`, and use `ets` or `counters` for anything shared that has to add up, such as counters.

These callbacks run for HTTP requests. Websocket handlers run their plugins through `nova_ws_handler`, which has its own contract; see `nova_websocket`.

""".

-type request_type() :: pre_request | post_request.
-export_type([request_type/0, reply/0]).

-type reply() :: {reply, Body :: iodata()} |
                 {reply, Status :: integer(), Body :: iodata()} |
                 {reply, Status :: integer(), Headers :: cowboy:http_headers(), Body :: iodata()} |
                 {redirect, Url :: binary() | string()} |
                 {status, Status :: integer()} |
                 {status, Status :: integer(), Headers :: cowboy:http_headers()}.

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
This function is called before the request is processed. It can modify the request
and the nova-state. See the module documentation for what it can return.
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
