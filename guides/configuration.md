# Configuration

Nova is configured through the application environment, normally in *sys.config*. This page lists every key Nova reads.

## The server and its listeners

Nova runs on a server package, `nova_cowboy` or `nova_roadrunner`, which
starts the listeners of the node. They are configured under the `listeners`
key of the `nova` application, each under a name:

```erlang
{nova, [
    {listeners, #{
        default => #{port => 8080}
    }}
]}
```

| Setting | Description | Value | Default |
|-----|-------------|-------|---------|
| `port` | Port to bind to | `integer()` | `8080`, or `8443` with `tls` |
| `ip` | IP to bind to | `tuple` | `{0, 0, 0, 0}` |
| `tls` | The options of the `ssl` application for a server. A listener with TLS needs at least a certificate and a key, for instance `#{certfile => "/path/to/fullchain.pem", keyfile => "/path/to/privkey.pem"}` | `map() \| list() \| false` | `false` |
| `protocols` | The HTTP versions the listener serves | `[http1 \| http2 \| http3 \| {Version, map()}]` | `[http1, http2]` with `tls`, `[http1]` without |
| `applications` | The Nova applications the listener serves | `[atom()]` | The bootstrap application, for the only listener |
| `steps` | The steps of a request, in order. The default is the Nova request pipeline, see [Request lifecycle](request-lifecycle.md). A list of your own replaces it | `[module()]` | `[nova_router, nova_plugin_handler, nova_security_handler, nova_handler, nova_plugin_handler]` |
| `server_opts` | Options of the server package. For Cowboy these are its protocol options, see `nova_cowboy`. For Roadrunner see `nova_roadrunner` | `map()` | `#{}` |

[Servers and listeners](listeners.md) describes them, with what a server can
be asked for and what is checked when the node starts.

The listeners are started by the application of the server package, after
everything a request needs is running. If one cannot be started, for
instance because the port is in use or a certificate cannot be read, Nova
logs `Could not start listener`, stops the listeners it had started and the
application of the server package fails to start with
`{listener_not_started, Reason}`.

The listeners go down with `nova_sup`: when it stops without the `nova`
application being stopped, for instance when it runs under a supervisor of
your own, the application of the server package stops as well. Start that
application again to serve again.

A node without `listeners` reads `cowboy_configuration`, the key of earlier
releases, as one listener named `default`. With neither key it has one
listener named `default` with the defaults above. See
[Servers and listeners](listeners.md#cowboy_configuration).


## Nova specific configurations

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `use_persistent_term` | When `false`, the compiled routing table is also added to the environment of the request steps under `dispatch` | `boolean()` | `true` |
| `dispatch_backend` | Module that stores the routing table and the plugin list. It has to export `get/1`, `get/2` and `put/2` as `persistent_term` does. Anything but a module name logs `dispatch_backend must be a module` and the default is used | `module()` | `persistent_term` |
| `server_grace` | How long after it started Nova warns that no server package has started a listener, in milliseconds. `infinity` for a node that serves nothing on purpose. Anything else stops the boot with `{invalid_setting, server_grace, Value}` | `non_neg_integer() \| infinity` | `10000` |
| `use_stacktrace` | If Nova should include the stacktrace in crash logs and, in the `dev` environment, on error pages. Read when the listener starts. What is redacted from both is listed under *Redaction in logs and error pages*. | `boolean()` | `false` |
| `redact_keys` | Extra key fragments whose values are redacted from crash logs and error pages, on top of the built-in list. Atoms, binaries or strings; a key matches when its name contains the fragment, see *Redaction in logs and error pages*. | `[atom() \| binary() \| string()]` | `[]` |
| `render_error_pages` | With `false`, the `404` and `500` pages that come with Nova send the bare status code to every client, without a body. The status code routes of your application run either way. See [Error handling](error-handling.md#the-default-pages) | `boolean()` | `true` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` | `true` |
| `session_manager` | Specify a module to use as the session manager. Defaults to `nova_session_ets`, which keeps sessions in memory: they do not survive a restart, and with several nodes and no sticky sessions users are signed out when they reach another node. Use a shared session store for those cases; see the [Sessions](sessions.md) guide. | `atom()` | `nova_session_ets` |
| `session_cookie_name` | Name of the session cookie. It must be an RFC 6265 token: ASCII letters, digits and the symbols a token allows such as `-`, `_` and `.`, but no spaces, `:`, `/`, `@`, `=` or quotes. Nova refuses to start with any other name, including an empty binary or a value that is not a binary. With a `__Host-` name the cookie is always `Secure`, with `Path=/` and no `Domain`; set a name without the prefix for a cookie that needs a `domain`, another `path` or `secure => false`. See the [Sessions](sessions.md#the-__host-cookie-name) guide. | `binary()` | `<<"__Host-session_id">>`, and `<<"session_id">>` when `environment` is `dev` |
| `session_cookie_opts` | Options for the session cookie, merged over the defaults. The cookie is `Secure` unless `environment` is `dev`, which an unset `environment` is not. Developing over plain HTTP without `{nova, [{environment, dev}]}` loses the session cookie. `path` and `domain` must be ASCII without control characters or `;`, or Nova refuses to start. It also refuses to start, outside `dev`, with a `domain`, a `path` other than `/` or `secure => false` while `session_cookie_name` is not set: the default name has the `__Host-` prefix, which rules them out. The error is `{invalid_setting, session_cookie_opts, {requires, session_cookie_name}}`. See the [Sessions](sessions.md) guide. | `cow_cookie:cookie_opts()` | `#{path => <<"/">>, http_only => true, same_site => lax, secure => <not dev>, max_age => <session_max_age>}` |
| `session_max_age` | Seconds a session lives after it was created. Sets the cookie's `Max-Age` and expires `nova_session_ets` entries. `infinity` turns the limit off. | `pos_integer() \| infinity` | `1209600` (14 days) |
| `max_sessions` | Sessions the `nova_session_ets` backend holds at most, on each node: an integer above 0, or `infinity` for no limit. In a full table a write that starts a session drops another one to make room: an expired one, then a provisional one, then the one nearest its expiry. A provisional write returns `{error, session_store_full}` when only sessions in use are left. See the [Sessions](sessions.md#when-the-session-store-is-full) guide. *Checked at boot.* | `pos_integer() \| infinity` | `1000000` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` | `false` |
| `environment` | The environment Nova runs in. It is passed to the `routes/1` callback of every router and returned by `nova:get_environment/0`. When it is not set it is read as `prod`: routers are asked for `routes(prod)`, and Nova logs one notice when it starts. Only `dev` turns on what is meant for development (`nova:is_dev_environment/0`): crash details on error pages, a session cookie without `Secure` over plain HTTP and the output of watchers. Set `{environment, dev}` in the development configuration. | `atom()` | not set, read as `prod` |
| `non_canonical_paths` | What happens to a request whose path is not written the way it is routed, such as `/a//b` or `/a/../b`. `reject` answers it with `400`. `resolve` routes it as earlier releases did. See [Routing](routing.md#paths-that-are-not-canonical). *Checked at boot.* | `reject \| resolve` | `reject` |
| `trusted_origins` | The origins a browser may open a websocket from, or send a request that changes state from. `any` opens websockets alone. See [Trusted origins](#trusted-origins). *Checked at boot.* | `same_host \| same_origin \| any \| [binary() \| string()]` | `same_host` |
| `secret_key_base` | The one secret of the node, from which Nova makes a key for the tokens of `nova_csrf_plugin` and a key for session ids. A binary of at least 32 bytes out of a random source, or a list of such, where the first signs and all of them verify. See [The secret of the node](#the-secret-of-the-node). *Checked at boot.* | `binary() \| [binary()]` | not set |
| `csrf_token` | How `nova_csrf_plugin` makes its tokens: `stateless` or `session`. When it is not set, tokens are `stateless` with a `secret_key_base` and `session` without one. `stateless` stops the boot without a `secret_key_base`, and with `use_sessions` set to `false`. See [Plugins](plugins.md#csrf-token-modes). *Checked at boot.* | `stateless \| session` | not set |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` | *Will crash if not defined* |
| `server` | The module of the server package Nova runs on. Only needed when the release has more than one | `module()` | `nova_cowboy` when it is in the release, else `nova_roadrunner` |
| `listeners` | The listeners of the node, see *The server and its listeners* above. | `map()` | One listener named `default` |
| `cowboy_configuration` | The one listener of releases before 1.0. Read when `listeners` is not set, with a notice at boot. | `map()` | `#{}` |
| `plugins` | The global plugins, a list of `{Phase, Module, Options}`. They run for every route group that declares no `plugins` of its own. See the [Plugins](plugins.md) guide. | `[{atom(), module(), map()}]` | `[]` |
| `plugins_before_security` | Plugin modules whose `pre_request/4` runs ahead of the security callback, as it did up to 0.18, for route groups that do not set the key themselves. `all`, or a list of modules that export `pre_request/4`. See [Plugins](plugins.md#running-a-plugin-ahead-of-the-security-callback). *Checked at boot.* | `all \| [module()]` | `[]` |
| `multipart_spool` | Bounds on the upload files spooled at one time by `nova_multipart_file_handler`: `max_files` and `max_bytes`, each an integer or `infinity`. A part past a bound gets a `503`. See the [Plugins](plugins.md#nova-multipart) guide. | `map()` | `#{max_files => infinity, max_bytes => 8000000000}` |
| `telemetry_request_fields` | Request fields to add to the controller telemetry metadata. `path` (the raw request path) and `peer` (the client's address) are the only ones accepted and are left out by default. See the [Telemetry](telemetry.md) guide. | `[path \| peer]` | `[]` |
| `shutdown_delay` | Milliseconds to wait before suspending the listener during shutdown. Useful for letting load balancers drain traffic. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `0` |
| `shutdown_drain_timeout` | Maximum milliseconds to wait for active connections to finish during shutdown. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `15000` |

### Where Nova reads a setting

`json_lib`, `use_sessions`, `session_cookie_name`, `session_cookie_opts`, `session_max_age` and `redact_keys` are read from the bootstrap application's environment first, then from the `nova` environment, then the default. A value set in your own application always wins:

```erlang
[{nova, [{bootstrap_application, my_app}]},
 {my_app, [{json_lib, json}, {use_sessions, true}]}].
```

The other keys in the table above are only read from `nova`.

### Settings checked at boot

The rows marked *Checked at boot* are read once, when Nova starts, and from the `nova` environment only. The same key in the environment of the bootstrap application is not read.

```erlang
{nova, [
    {bootstrap_application, my_app},
    {environment, prod},
    {non_canonical_paths, reject},
    {max_sessions, 200000},
    {plugins_before_security, []}
]}
```

A value that cannot be used stops the boot. Nova logs an error that says what the setting may be, and `nova_sup` fails to start with `{invalid_setting, Key, Reason}`:

| `Reason` | When |
|---|---|
| The value that was refused | Every setting but the secret |
| `redacted` | `secret_key_base` cannot be used, see [The secret of the node](#the-secret-of-the-node). The value is not logged |
| `not_set` | `csrf_token` is `stateless` and there is no secret to sign with. The key in the error is `secret_key_base` |
| `{requires, Setting}` | The setting cannot be used without another one. `{invalid_setting, csrf_token, {requires, use_sessions}}` is `csrf_token` set to `stateless` on a node without sessions |
| `{replaced_by, Setting}` | The key is one that was in no release and is gone: `allowed_origins` is now `trusted_origins`, `csrf_secret` is now `secret_key_base`, and `plugins_after_security` gave way to `plugins_before_security`. Nova does not start while one of them is set, see [Deprecations](deprecations.md) |

```erlang
{nova, [{max_sessions, 0}]}
%% {invalid_setting, max_sessions, 0}
```

What a request reads is the value that was checked. `application:set_env(nova, Key, Value)` on a running node has no effect on these settings until Nova is restarted.

Three checks are made when the routes of an application are compiled, and stop the application from starting with the same kind of error:

| Error | When |
|---|---|
| `{invalid_setting, use_sessions, {required_by, nova_csrf_plugin}}` | A route group lists `nova_csrf_plugin` and `use_sessions` is `false` |
| `{invalid_setting, security_reads_body, {requires, plugins_before_security}}` | A route group says `security_reads_body => true` and its body is decoded behind the security callback, see [Plugins](plugins.md#saying-whether-the-security-callback-reads-the-body) |
| `{invalid_setting, Key, Value}` or `{invalid_setting, Key, {replaced_by, Setting}}` | A route group has `plugins_before_security`, `security_reads_body` or the `trusted_origins` option of a plugin with a value that cannot be used, or still has `plugins_after_security` or the plugin option `allowed_origins` |

### The secret of the node

`secret_key_base` is one secret for the whole node. Nova never signs with it as it is. It makes a key of 32 bytes for each purpose from it, so what is signed for one purpose says nothing for another:

| Key for | Used by |
|---|---|
| CSRF tokens | `nova_csrf_plugin`, which signs its tokens and stores nothing for them, see [Plugins](plugins.md#csrf-token-modes) |
| Session ids | `nova_session`, which signs the ids it makes, so a visitor keeps the id a page handed out, see [Sessions](sessions.md#the-session-cookie) |

```erlang
{nova, [
    {secret_key_base, ~"${SECRET_KEY_BASE}"}
]}
```

That is a line of `sys.config.src`: the release puts the environment variable `SECRET_KEY_BASE` in when the node starts, see [Building releases](building-releases.md). Give every node the same secret and keep it across restarts. It does not belong in a file that is committed. Use it for Nova alone: an application that signs things of its own has a secret of its own.

The secret has to come out of a random source. Whoever sees a signed session id or token can try secrets against it on a machine of their own, as fast as HMAC runs, so a word or a sentence does not do, however long. Make one in an Erlang shell:

```erlang
base64:encode(crypto:strong_rand_bytes(48)).
```

or with `openssl`:

```sh
openssl rand -base64 48
```

Both give 64 characters.

Nova does not start, with `{invalid_setting, secret_key_base, redacted}`, when the secret, or one secret of a list, is any of these:

| Refused | Example |
|---|---|
| Not a binary, or an empty list | `"a string in double quotes"`, `[]` |
| Shorter than 32 bytes | `~"too short"` |
| Fewer than 8 different bytes | A letter or a short word repeated |
| An example out of this documentation | The sample secrets earlier versions of these guides printed |
| A variable that nothing filled in | `~"${SECRET_KEY_BASE}"` reaching Nova as it is written, because the release did not replace it |

Nova cannot tell a sentence from a random value, so a secret that passes these checks is not shown to be a good one. The secret is never logged, and what Nova keeps of it once it has started is the keys made from it.

To change the secret, set a list. The first one signs and every one of them verifies:

```erlang
{nova, [
    {secret_key_base, [~"${SECRET_KEY_BASE}", ~"${SECRET_KEY_BASE_OLD}"]}
]}
```

Put the new secret in front of the old one, on every node, and take the old one out once the pages that were open have been sent. A session that is stored is not touched by the change. A visitor whose session cookie was signed with a secret that is gone, and who has no session yet, gets a new id with the first write to the session.

### Trusted origins

`trusted_origins` is one setting with two readers. Nova checks every request to a websocket route against it, and `nova_csrf_plugin` checks a request that changes state. A request with neither an `Origin` nor a `Sec-Fetch-Site` header passes every value: it is not from a page.

```erlang
{nova, [
    {trusted_origins, [~"https://app.example.com", ~"https://admin.example.com"]}
]}
```

| Value | An origin passes when |
|---|---|
| `same_host` | It names the host and the port the request was sent to. On a request that came in over TLS it also has to be an `https` origin. On a listener without TLS the scheme is not compared. This is the default |
| `same_origin` | It has the scheme, the host and the port of the request as Nova received it |
| A list of origins | It is in the list. Letter case is ignored, nothing else is: `https://app.example.com:443` is not `https://app.example.com` |
| `any` | Always, on a websocket route. `nova_csrf_plugin` reads `any` of the `nova` environment as `same_host` |

With `same_host` the host and port come from the `Host` header of the request, or from the authority of an HTTP/2 or HTTP/3 request that has no such header. Ports are compared like this:

| `Host` header | `Origin` | Result |
|---|---|---|
| `app.example.com` | `https://app.example.com` | Passes |
| `app.example.com` | `https://app.example.com:443` | Passes: 443 is the port of `https` |
| `app.example.com` | `https://app.example.com:8443` | Refused |
| `app.example.com:443` | `https://app.example.com` | Passes: the origin leaves out the port of its scheme |
| `app.example.com:8443` | `https://app.example.com:8443` | Passes |
| `app.example.com:8443` | `https://app.example.com` | Refused |

The scheme is compared with `same_host` only where Nova can see it. A listener with TLS refuses an origin of any other scheme than `https`, `http://app.example.com` and `wss://app.example.com` included. A listener without TLS takes an origin of either scheme, since a proxy in front of it may have taken the TLS off. Nova reads no `X-Forwarded-Proto` or `Forwarded` header to find out: it has no setting that says which proxy to believe.

A list takes the place of `same_host`. An origin that is not in it is refused, the origin of the site itself included, so list that one too. `[]` passes no origin.

`same_origin` compares with what Nova received. Behind a proxy that terminates TLS that is `http`, so every `https` origin is refused. Use a list of the `https` origins of the site there. `same_host` passes them too, with the risk described in [Security](security.md#behind-a-proxy-that-terminates-tls).

A browser also says where the page that sends a request is, in the `Sec-Fetch-Site` header, which no page can set. When the header is there and says the page is elsewhere, with any value but `same-origin` and `none`, comparing the origin with the host has nothing to add, so `same_host` and `same_origin` pass nothing: the origin has to be in a list, or the value has to be `any`. Not every browser sends the header with a websocket upgrade, see [Websockets](websockets.md#the-origin-check). What `nova_csrf_plugin` does with each combination of the two headers is in [Plugins](plugins.md#csrf-trusted-origins).

`any` in the `nova` environment opens websocket upgrades to every origin and nothing else. `nova_csrf_plugin` then checks by `same_host`. Do not set it to get websockets through a proxy that rewrites the `Host` header: list the origins instead. To turn the origin check of `nova_csrf_plugin` off and leave a request to its token alone, give the plugin of that route group the option `#{trusted_origins => any}`, see [Plugins](plugins.md#csrf-trusted-origins).

### Redaction in logs and error pages

Crash reasons, plugin and controller results, and stacktraces can hold request data. Before Nova writes them to a log line or a crash report, or renders them on an error page, it:

- replaces every request map with its method and path, so headers, cookies, the query string and the body are left out;
- replaces the argument list of every stacktrace frame with its arity;
- replaces the value of every sensitive key with `redacted`, in maps and in lists of `{Key, Value}` pairs such as proplists and header or query string lists.

A key is sensitive when its name contains one of these fragments: `passw`, `secret`, `token`, `apikey`, `authorization`, `cookie`, `session`, `csrf`, `privatekey`, `credential`, or a fragment listed in `redact_keys`. Two more match as whole names only: `qs` (so the query string of the partial request a websocket's `terminate/3` gets is redacted too) and `otp`. Keys can be atoms, binaries or strings. Names are compared in lower case with `-` and `_` removed, so `access_token`, `X-Api-Key`, `clientSecret`, `password_confirmation` and `Set-Cookie` are all covered. Only the first 64 bytes of a name are looked at.

A fragment in `redact_keys` matches everywhere it occurs: `pin` also redacts `shipping`. Prefer the longest fragment that still covers your keys.

A `{Key, Value}` tuple that is not an element of a list has the same shape as an error tuple, so its value is redacted only when the whole name is one of the fragments above, or is `password`, `session_id`, `set-cookie`, `_csrf_token`, `x-csrf-token`, `qs` or `otp`: `{token, T}` is redacted, `{invalid_token, Reason}` is not.

Anything else is logged as is: a secret held under another key, or in a plain tuple or a binary, still reaches the log. Add such keys to `redact_keys`. Values your own code logs are not touched, unless the logger formats with `nova_jsonlogger` and its `redact` option is set: `{nova_jsonlogger, #{redact => true}}` applies the same rules to every report and its metadata, and to the arguments of an event logged as a format string with arguments, before they are put into the text. A secret that is text already is not found: a message logged as a plain string, the format string itself, and anything inside a binary, such as the unparsed bytes of a request in a crash report. The option is off by default.

## Application parameters

These parameters are set in the environment of your own application, not under `nova`. `watchers` is only read from the application named in `bootstrap_application`. `router_module` and `nova_apps` are read from every application Nova mounts.

| Key | Description | Value |
|-----|-------------|-------|
| `json_lib` | JSON lib to use. Read more in the subsection *Configure json lib* | `atom()` |
| `watchers` | Watchers are external programs that will run together with Nova, in every environment. Each one is a tuple `{Command}`, `{Command, Arguments}`, `{Command, Options}`, `{App, Command, Options}` or `{App, Command, Arguments, Options}` (Like `[{my_app, "npm", ["run", "watch"], #{workdir => "priv/assets/js/my-app"}}]`). See the [Watchers](watchers.md) guide. | `[{string()}] | [{string(), [string()]}] | [{string(), map()}] | [{atom(), string(), map()}] | [{atom(), string(), [string()], map()}]` |
| `router_module` | Module that contains the `routes/1` callback. Defaults to the application name followed by `_router`, such as `my_app_router` | `atom()` |
| `nova_apps` | Other Nova applications to serve from the same listener, each an application name or `{Name, Options}`. See [Multiple applications](multi-app.md) | `[atom() \| {atom(), map()}]` |

Nova writes a key `apps` to the environment of the bootstrap application for its own use. Do not set it.


### Configure json_lib

Nova encodes and decodes JSON with the `json` module that ships with Erlang/OTP 27 and later, so it needs no JSON dependency. With this default a non-empty list whose first element is a pair is encoded as an object: `[{a, 1}]` becomes `{"a":1}`. An object that has a key twice does not decode, and neither does a value that is followed by anything but whitespace: `decode_json_body` answers such a body with `400`. A `json_lib` of your own decides both itself.

To use another library, add it to your application's dependencies and set `json_lib` in your bootstrap application's environment; Nova also reads it from the `nova` environment when the bootstrap application does not set it.

```erlang
{my_app, [{json_lib, thoas}]}
```

Nova used `thoas` by default up to 0.18 and no longer depends on it. An application that calls `thoas` itself, or wants to keep it as `json_lib`, has to list it in its own `deps`.

The module defined for this should expose two different functions:

`encode(Structure) -> binary() | iolist()`

`decode(JsonString) -> {ok, Structure} | {error, Reason} | Structure`

`decode/1` may return `{ok, Structure}` (like `thoas`) or the structure itself and raise on bad input (like OTP's `json` module), so either kind of library works without an adapter.


## Handling errors in Nova

A request that fails is answered with a status page. Which page, what it
contains and how to replace it is described in
[Error handling](error-handling.md). The settings involved are
`render_error_pages`, `use_stacktrace` and `environment` in the table above.
