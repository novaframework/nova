-module(nova_ws_origin_plugin).
-moduledoc """
Refuses a websocket upgrade that was started by a page on another origin.

A browser sends the cookies of a site with every websocket upgrade to it, whichever page opened the connection. A websocket that trusts a session cookie should therefore check the `Origin` header of the upgrade. This plugin answers such an upgrade with `403` and lets every other request through.

A request without an `Origin` header is let through, since only browsers send it. A browser also says where the page that opens the websocket is, in `Sec-Fetch-Site`, which no page can set. When that header is there and is neither `same-origin` nor `none`, the page is on another site, or on the same site under another scheme or name, and the upgrade is refused unless the `Origin` is in a list of trusted origins, or the origins are `any`: the rule `nova_csrf_plugin` has for a request that changes state. When the header is `same-origin` or `none`, or is not sent, the `Origin` decides as described below. A client that sends neither header, which is what a client that is not a browser does, is let through as it always was.

Nova runs this plugin first on every websocket route, whether the route group lists it or not, against `trusted_origins` of the `nova` environment, which takes the values of the option below and is `same_host` when it is not set. A route group that lists the plugin with its own `trusted_origins` is checked against those alone, so listing the plugin is how one group gets other origins than the rest. `any` turns the check off, for the route group or for every route:

```erlang
{nova, [
    {trusted_origins, [~"https://example.com"]}
]}
```

`nova_csrf_plugin` reads the same setting for the requests it checks. It is not `allow_origins` of `nova_cors_plugin`: that one says which pages may read a response, this one which pages may act with the cookies of the site.

The check is made ahead of the security callback and of every other plugin of the route, wherever among the plugins of the route group this one is listed. A request that is refused is answered `403` with nothing of the route having run.

## Options

- `trusted_origins` - `trusted_origins` of the `nova` environment when the option is left out. `same_host` (the default of that) accepts an origin whose host and port are the ones in the `Host` header, or the ones the request was sent to when it has no `Host` header, as over HTTP/2. A request that came in over TLS also needs an `https` origin. On a listener without TLS the scheme is not compared, apart from the port an origin leaves out being the one of its scheme: a proxy in front of it may have taken the TLS off, and Nova reads no forwarded header to find out. `same_origin` also compares the scheme: the origin must have the scheme, host and port of the request, where a port that is left out is the one of the scheme. A list accepts exactly the origins in it, such as `[~"https://example.com"]`, also from a page the browser says is on another site. It takes the place of `same_host`, so the origin of the site itself has to be in the list to be accepted, and `[]` accepts none. Use a list when a proxy in front of Nova rewrites the `Host` header or terminates TLS. `any` accepts every origin, whatever `Sec-Fetch-Site` says. Any other value stops the boot with `{invalid_setting, trusted_origins, Value}`.

## Behind a proxy that terminates TLS

The listener of Nova is then one without TLS, where `same_host` does not compare the scheme, and `same_origin` compares it with `http`, which is not the scheme of the site. A page loaded from `http://example.com` has the host of a site served at `https://example.com`, and it is not the site: anybody on the network between a visitor and the server can answer for `http://example.com` and write that page, unless the browser already knows by HSTS that the host is only to be asked over TLS. Such a page opening `wss://example.com` sends the cookies of the visitor with the upgrade.

A browser that sends `Sec-Fetch-Site` with the upgrade says that this page is another site, and the upgrade is refused. Not every browser sends it with a websocket upgrade: Firefox does and Safari does from 16.4, while Chrome and the browsers built on it have been reported not to. A browser that does not send the header is taken by its `Origin` alone, and under `same_host` that page is let in: it can open the websocket as the visitor and read what the websocket sends. So the header narrows this risk and does not end it.

Behind such a proxy list the origins of the site, with their scheme, which closes it for every browser:

```erlang
{nova, [
    {trusted_origins, [~"https://example.com"]}
]}
```

and send `Strict-Transport-Security` with `includeSubDomains` from the proxy, so that a browser that has been to the site does not load a page of it without TLS at all. Nova reads no `X-Forwarded-Proto` or `Forwarded` header to find out what the proxy did, since it has no setting that says which proxy to believe.

Neither `same_host` nor `same_origin` stops DNS rebinding, where another name is pointed at the address of the server: the origin and the host of such a request are the same name. A list of origins does, since the other name is not in it.

## Example configuration

The plugin only acts on websocket upgrades, so it can go in the global `plugins` of the `nova` application:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_ws_origin_plugin, #{trusted_origins => [~"https://example.com"]}}
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
-spec pre_security(Req :: nova_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: nova_req:req(), NewState :: any()} |
          {stop, nova_plugin:reply(), Req0 :: nova_req:req(), NewState :: any()}.
pre_security(Req, Env, Options, State) ->
    Trusted = case Options of
                  #{trusted_origins := Origins} -> Origins;
                  _ -> nova_settings:value(trusted_origins)
              end,
    case is_for_a_websocket(Req, Env) andalso not is_from_a_trusted_page(Req, Trusted) of
        true -> {stop, {reply, 403, ~"Origin not allowed"}, Req, State};
        false -> {ok, Req, State}
    end.

%% `Sec-Fetch-Site` is the browser saying where the page that opens the
%% websocket is, and no page can set it. When it says the page is somewhere
%% else, comparing the origin with the host of the request has nothing to
%% add: behind a proxy that takes the TLS off, a page loaded without TLS has
%% the host of the site. Only an origin that is listed is then let in, which
%% is the rule nova_csrf_plugin has for a request that changes state. A
%% browser that says the page is of the site, or says nothing, is taken by
%% its `Origin`, and a client that sends neither header is not a page.
-spec is_from_a_trusted_page(Req :: nova_req:req(), Trusted :: term()) -> boolean().
is_from_a_trusted_page(Req, Trusted) ->
    Origin = nova_req:header(~"origin", Req),
    case nova_req:header(~"sec-fetch-site", Req) of
        Site when Site =:= undefined; Site =:= ~"same-origin"; Site =:= ~"none" ->
            nova_origin:is_allowed(Origin, Req, Trusted);
        _Elsewhere ->
            Trusted =:= any orelse
                (is_binary(Origin) andalso is_list(Trusted) andalso nova_origin:is_allowed(Origin, Req, Trusted))
    end.

-doc false.
-spec post_request(Req :: nova_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: nova_req:req(), NewState :: any()}.
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
      options => [{trusted_origins,
                   ~"same_host, same_origin, any, or the list of origins that may open a websocket"}]}.

%% Every request to a websocket route is checked, also one that does not ask
%% for the upgrade: its controller is started before the server turns it down.
is_for_a_websocket(_Req, #{handler_module := nova_ws_handler}) ->
    true;
is_for_a_websocket(Req, _Env) ->
    is_websocket_upgrade(Req).

%% A request whose headers do not parse is checked too.
is_websocket_upgrade(Req) ->
    try
        nova_req:is_websocket_upgrade(Req)
    catch
        _Class:_Reason -> true
    end.
