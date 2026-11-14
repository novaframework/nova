-module(nova_ws_origin_plugin).
-moduledoc """
Refuses a websocket upgrade that was started by a page on another origin.

A browser sends the cookies of a site with every websocket upgrade to it, whichever page opened the connection. A websocket that trusts a session cookie should therefore check the `Origin` header of the upgrade. This plugin answers such an upgrade with `403` and lets every other request through.

A request without an `Origin` header is let through, since only browsers send it.

## Options

- `allowed_origins` - `same_host` (the default) accepts an origin whose host and port are the ones in the `Host` header. A list accepts exactly the origins in it, such as `[~"https://example.com"]`. Use a list when a proxy in front of Nova rewrites the `Host` header.

## Example configuration

```erlang
#{prefix => "/live",
  plugins => [{pre_request, nova_ws_origin_plugin, #{allowed_origins => [~"https://example.com"]}}],
  routes => [{"/ws", my_ws_controller, #{protocol => ws}}]}
```
""".
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

-doc "Stops a websocket upgrade from an origin that is not allowed with a 403.".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
          {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    Allowed = maps:get(allowed_origins, Options, same_host),
    case is_websocket_upgrade(Req) andalso not is_allowed(cowboy_req:header(~"origin", Req), Req, Allowed) of
        true -> {stop, {reply, 403, ~"Origin not allowed"}, Req, State};
        false -> {ok, Req, State}
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
      options => [{allowed_origins, ~"same_host, or the list of origins that may open a websocket"}]}.

is_websocket_upgrade(Req) ->
    Upgrade = cowboy_bstr:to_lower(cowboy_req:header(~"upgrade", Req, <<>>)),
    binary:match(Upgrade, ~"websocket") =/= nomatch.

is_allowed(undefined, _Req, _Allowed) ->
    true;
is_allowed(Origin, Req, same_host) ->
    case binary:split(cowboy_bstr:to_lower(Origin), ~"://") of
        [_Scheme, Authority] -> Authority =:= cowboy_bstr:to_lower(cowboy_req:header(~"host", Req, <<>>));
        _NoAuthority -> false
    end;
is_allowed(Origin, _Req, Allowed) when is_list(Allowed) ->
    LowerOrigin = cowboy_bstr:to_lower(Origin),
    lists:any(fun(AllowedOrigin) ->
                      cowboy_bstr:to_lower(iolist_to_binary(AllowedOrigin)) =:= LowerOrigin
              end, Allowed).
