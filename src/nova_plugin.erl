%%%-------------------------------------------------------------------
%%% Copyright (C) 2020, Niclas Axelsson
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
-module(nova_plugin).
-moduledoc """
Plugins can be run at two different times; either in the beginning or at the end of a request. They can modify both the actual request or the nova-state. A plugin is implemented with the nova_plugin behaviour and needs to implement three different functions: pre_request/2, post_request/2 and plugin_info/0.

A plugin can return either \{ok, NewState\}, \{break, NewState\}, \{stop, NewState\}, \{error, Reason\}.

\{ok, NewState\} will continue the normal execution with the NewState.

\{break, NewState\} breaks the execution of the current plugin-chain. This means that if a pre_request-plugin returns the break-statement the rest of the plugins in that chain will be skipped.

\{stop, NewState\} stops the execution. The plugin is responsible in this case for returning a proper response to the client.

\{error, Reason\} will stop the execution and call the nova_error plugin, resulting in a 500 response back to the user. If debug mode is enabled the reason will be returned in the 500-response.

## Before and after the security callback

A plugin listed as `{pre_request, Module, Options}` takes part twice before the controller, with the same options:

- `pre_security/4`, when the module has it, runs ahead of the security callback of the route, for every request to the route. It is for what has to happen whether or not the request is let in: answering a CORS preflight, which carries no credentials, giving the request its correlation id, turning a client away by address or rate. It must not read the request body. It is also all that runs ahead of the `405` for a CORS preflight to a route that does not take `OPTIONS`, of the plugins of the route for the method the preflight asks about.
- `pre_request/4` runs once the security callback has let the request through, and at once on a route without one. Reading and decoding the body belongs here, so that nothing is read from a client that is refused.

Earlier releases ran `pre_request/4` ahead of the security callback. `plugins_before_security`, in a route group or in the `nova` environment (the route group wins), puts it back there for the modules it names: `pre_request/4` of these modules runs ahead of the security callback, as it did. `all` names every plugin of the route group, and `all` in the `nova` environment is the order of earlier releases everywhere. A list has to name plugins that have `pre_request/4`, and the list of a route group plugins that the group lists as `pre_request`: any other name stops the boot with `{invalid_setting, plugins_before_security, List}`. Nothing moves `pre_security/4` behind the security callback.

`auth_data` of the request is what the security callback returned as `{true, AuthData}`, and a plugin that writes the key, in either callback, does not change that. What a plugin writes there stays when the security callback returns a plain `true` and on a route without one. A websocket controller gets the `auth_data` of the upgrade request in its controller data.

Within each of the two phases the plugins run in the order they are listed. What the security callback finds in the request is what `pre_security/4` put there. With the plugins of Nova that is the correlation id, the CORS headers and `parsed_qs`, and not `json`, `params`, `body` or `files`: a security callback that needs the decoded body lists `nova_request_plugin` under `plugins_before_security` for its route group.
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
It takes a state as argument and should return ok.
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
    {stop, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {error, Reason :: term()}.
-optional_callbacks([pre_request/4]).

-doc """
This function is called after the request is processed. It can modify the request.
It takes a state and a map of options as arguments and should return
either `{ok, NewState}`, `{break, NewState}`, `{stop, NewState}` or `{error, Reason}`.
The state is only used if there's another plugin invoked after this one.
""".
-callback post_request(Req :: cowboy_req:req(), Env :: any(),
                       Options :: map(), PluginState :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {ok, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Req0 :: cowboy_req:req(), NewState :: any()} |
    {break, Reply :: reply(), Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
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
