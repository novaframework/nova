%%%-------------------------------------------------------------------
%%% Copyright (C) 2020, Niclas Axelsson
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
-module(nova_plugin).
-moduledoc """
Plugins can be run at two different times; either in the beginning or at the end of a request. They can modify both the actual request or the nova-state. A plugin is implemented with the nova_plugin behaviour and needs to implement pre_request/4 and/or post_request/4 and plugin_info/0.

A plugin returns one of:

`{ok, Req, State}` continues with the next plugin.

`{ok, Reply, Req, State}` sets the reply on `Req` and continues.

`{break, Req, State}` and `{break, Reply, Req, State}` skip the rest of the current plugin chain. If a pre_request plugin breaks, the remaining pre_request plugins are skipped and the request carries on to the security function and the controller.

`{stop, Req, State}` and `{stop, Req}` stop the execution. The plugin is responsible in this case for returning a proper response to the client. `{stop, Reply, Req, State}` stops and sends `Reply`.

`{error, Reason}` stops the execution and renders the 500 status page.

When a pre_request plugin stops the request, or the security function refuses it, the controller and the post_request plugins do not run.

""".

-type request_type() :: pre_request | post_request.
-export_type([request_type/0, reply/0]).

-type reply() :: {reply, Body :: binary()} |
                 {reply, Status :: integer(), Body :: binary()} |
                 {reply, Status :: integer(), Headers :: [{binary(), binary()}], Body :: binary()}.

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
