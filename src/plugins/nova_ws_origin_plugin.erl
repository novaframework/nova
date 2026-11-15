-module(nova_ws_origin_plugin).
-moduledoc """
Refuses a websocket upgrade that was started by a page on another origin.

A browser sends the cookies of a site with every websocket upgrade to it, whichever page opened the connection. A websocket that trusts a session cookie should therefore check the `Origin` header of the upgrade. This plugin answers such an upgrade with `403` and lets every other request through.

A request without an `Origin` header is let through, since only browsers send it.

Nova makes this check for every websocket route, with or without the plugin, against `allowed_origins` of the `nova` environment, which takes the values of the option below and is `same_host` when it is not set. A route group that lists the plugin with its own `allowed_origins` is checked against those alone, so the plugin is how one group gets other origins than the rest. `any` turns the check off, for the route group or for every route:

```erlang
{nova, [
    {allowed_origins, [~"https://example.com"]}
]}
```

`nova_csrf_plugin` reads the same setting for the requests it checks.

The check of a route without the plugin is made when the upgrade is about to happen, after the plugins and the security callback of the route. The plugin makes it ahead of the security callback.

## Options

- `allowed_origins` - `allowed_origins` of the `nova` environment when the option is left out. `same_host` (the default of that) accepts an origin whose host and port are the ones in the `Host` header, or the ones the request was sent to when it has no `Host` header, as over HTTP/2. Only host and port are compared, not the scheme, apart from the port an origin leaves out being the one of its scheme. `same_origin` also compares the scheme: the origin must have the scheme, host and port of the request, where a port that is left out is the one of the scheme. A list accepts exactly the origins in it, such as `[~"https://example.com"]`. Use a list when a proxy in front of Nova rewrites the `Host` header or terminates TLS. `any` accepts every origin.

With `same_host` a page loaded from `http://example.com` can open a websocket to `wss://example.com` over HTTP/1.1. `same_origin` refuses it.

Neither `same_host` nor `same_origin` stops DNS rebinding, where another name is pointed at the address of the server: the origin and the host of such a request are the same name. A list of origins does, since the other name is not in it.

## Example configuration

The plugin only acts on websocket upgrades, so it can go in the global `plugins` of the `nova` application:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_ws_origin_plugin, #{allowed_origins => [~"https://example.com"]}}
    ]}
]}
```

A route group that declares `plugins` uses only those and none of the global ones. When the plugin is put on a route group, list every other plugin the group needs with it.
""".
-behaviour(nova_plugin).

-export([pre_security/4,
         post_request/4,
         plugin_info/0]).

-doc "Stops a websocket upgrade from an origin that is not allowed with a 403, ahead of the security callback.".
-spec pre_security(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
          {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_security(Req, _Env, Options, State) ->
    Allowed = case Options of
                  #{allowed_origins := Origins} -> Origins;
                  _ -> nova_settings:value(allowed_origins)
              end,
    case is_websocket_upgrade(Req) andalso not nova_origin:is_allowed(cowboy_req:header(~"origin", Req), Req, Allowed) of
        true -> {stop, {reply, 403, ~"Origin not allowed"}, Req, State};
        false -> {ok, Req#{nova_ws_origin => decided}, State}
    end.

-doc false.
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-doc false.
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => ~"nova_ws_origin_plugin",
      version => ~"0.1.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Refuses websocket upgrades from other origins",
      options => [{allowed_origins,
                   ~"same_host, same_origin, any, or the list of origins that may open a websocket"}]}.

%% The check Cowboy makes before it upgrades, which also knows the extended
%% CONNECT of HTTP/2 and HTTP/3. A request it cannot parse is checked too.
is_websocket_upgrade(Req) ->
    try
        cowboy_websocket:is_upgrade_request(Req)
    catch
        _Class:_Reason -> true
    end.
