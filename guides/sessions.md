# Sessions

Sessions in Nova are handled by the `nova_session` module. This module is responsible for setting and getting session-values, as well as managing the session data. This means that you can store data in sessions without having to worry about session ids or anything like that - it's all handled for you.

## API

### `nova_session:set/3`

```erlang
-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: binary()) ->
                 ok | {error, Reason :: atom()} | no_return().
```

This function is used to set a session value. It takes two arguments: the request object, the name of the session-key and the value.

```erlang
nova_session:set(Req, "name", "John Doe").
```

### `nova_session:get/2`

```erlang
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
          {ok, Value :: binary()} | {error, Reason :: atom()} | no_return().
```

This function is used to get a session value. It takes two arguments: the request object and the name of the session-key.

```erlang
nova_session:get(Req, "name").
```

### `nova_session:delete/2`

```erlang
-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} |
                                                          {error, Reason :: atom()} | no_return().
```

This function is used to delete a value. It takes two arguments: the request object and the name of the value. The rest of the session is kept.

```erlang
nova_session:delete(Req, "name").
```

### `nova_session:delete/1`

```erlang
-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} |
                                           {error, Reason :: atom()}.
```

This function deletes the whole session and clears the session cookie. The returned request carries a fresh session id, so a later `set/3` on it in the same request starts a new session instead of reviving the deleted one.

```erlang
{ok, Req1} = nova_session:delete(Req).
```

### `nova_session:renew/1`

```erlang
-spec renew(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
```

Moves the client to a new, empty session: the old session's data is deleted and the response carries a new session cookie. Call it whenever the privilege level of the client changes, at sign-in, sign-out and password change. Calling it at sign-in is the defence against session fixation: a session id that was planted or observed before that point stops working. Use the returned request for the `set/3` calls that follow.

```erlang
login(Req) ->
    {ok, Req1} = nova_session:renew(Req),
    ok = nova_session:set(Req1, <<"user_id">>, UserId),
    {redirect, "/"}.
```

The cookie is written by `nova_stream_h`, so it reaches the client even when the controller does not return `Req1`. This works for changes made by the process serving the request, which is where controllers, plugins, security functions and `nova_websocket:init/1` run (a session written in `init/1` gets its cookie on the `101` response). A process spawned with the request can still read and write session data, but no cookie is sent for what it does.

A `stream_handlers` configuration without `nova_stream_h` keeps storing session data, and `renew/1` and `delete/1` then set or expire the cookie on the request they return, with the same attributes. Return that request, for example with `{json, 200, #{}, Req1, Body}` or `{redirect, Path, Req1}`, or the cookie is lost. A `set/3` from a client without a cookie does not start a session there, and presented ids are used without the check described below.

## The session cookie

The session id is a random 256-bit value from `crypto:strong_rand_bytes/1`, sent in the `session_id` cookie (`session_cookie_name` changes the name). `nova_stream_h` issues the cookie with the response of the first request that writes to the session, so visitors who never store anything get no cookie and their responses stay cacheable.

An id the client presents that the backend does not know (never issued, deleted or expired) is ignored and a new one is used, so a client cannot simply choose its own session id. This is defence in depth; `renew/1` at sign-in is what protects against fixation. The check runs in the request process the first time the request uses the session, not in the connection process.

A request that carries two or more different values for the session cookie, for example because a cookie of the same name was set for a parent domain or another path, is treated as having no session, and a warning is logged once per node.

The cookie is sent with `Path=/`, `HttpOnly`, `SameSite=Lax`, `Max-Age` set to `session_max_age`, and with `Secure` unless `{nova, [{environment, dev}]}` is set (a dev request over HTTPS still gets `Secure`). Behind a proxy that terminates TLS Nova sees plain HTTP, which is why `Secure` does not depend on the scheme. A staging environment served over plain HTTP that is not marked `dev` gets a cookie the browser never sends back, so sessions do not work there until you set `secure => false` in `session_cookie_opts` or serve it over HTTPS:

```erlang
{nova, [
    {session_cookie_opts, #{secure => false}},
    {session_max_age, 86400}
]}
```

### The `__Host-` cookie name

A cookie named with the `__Host-` prefix is only accepted by browsers when it is `Secure`, has `Path=/` and no `Domain`, so a sibling or parent domain cannot set or shadow it. New applications should use it:

```erlang
{nova, [{session_cookie_name, <<"__Host-session_id">>}]}
```

With a `__Host-` name Nova always sends `Secure` and `Path=/` and drops `domain`, whatever `session_cookie_opts` says. Changing the name of an existing application signs every client out once: the old `session_id` cookie is no longer read, so clients get a new session under the new name. The old cookie stays in the browser until it expires.

`session_cookie_opts` takes the options of `cowboy_req:set_resp_cookie/4` (`path`, `domain`, `secure`, `http_only`, `same_site`, `max_age`) and is merged over the defaults above. Other keys, and values of the wrong type (a `path` that is not a binary, for example), are ignored. The cookie name must be an RFC 6265 token and `path` and `domain` plain ASCII without control characters or `;`; Nova checks both at boot and refuses to start otherwise, as it does for a name that is empty or not a binary. Changed at runtime to something invalid, `nova_stream_h` (one of the default `stream_handlers`) leaves the cookie off the response and logs one error per cookie name. Without `nova_stream_h`, `renew/1` and `delete/1` set the cookie themselves, and an invalid setting makes them raise, so the request gets a 500.

`session_max_age` (seconds, 14 days by default) limits how long a session lives from the moment it was created, whatever happens in between. It sets `Max-Age` on the cookie, and the default `nova_session_ets` backend drops the session once it has passed. With `infinity` the cookie has no `Max-Age` and sessions live until they are deleted or the node restarts.

The default `nova_session_ets` backend keeps sessions in memory, replicated to the nodes connected at the time of the write. They do not survive a restart, and a node that joins later does not have the sessions written before it joined. Because unknown ids are replaced, a client that reaches such a node (during a rolling deploy, or behind a load balancer without sticky sessions) gets a fresh, empty session and is signed out. Use a shared backend when sessions have to outlive a node.

## Enabling Sessions

Sessions are enabled by default in Nova, but if you need to disable them, you can do so by setting the `use_sessions` configuration option to `false`. This will disable sessions for the entire site. Like `json_lib`, it is read from your bootstrap application's environment first and then from `nova`'s (see the [configuration guide](configuration.md)).

It's also possible to set the backend for the session data. By default, the session data is stored in an *ETS* table in the Erlang VM but it's quite easy to implement a custom backend.

## Backend

You can implement your own backend by creating a module that implements the `nova_session` behaviour. This behaviour has four functions that need to be implemented: `get_value/2`, `set_value/3`, `delete_value/1` and `delete_value/2`.

Implement the optional `session_exists/1` too. Nova calls it for the session id a client presents and replaces ids it returns `false` for. It is called on every `get/2`, `set/3` and `delete` of a request that presented a cookie, so keep it as cheap as a key lookup. A backend without it accepts any id a client sends, and Nova logs a warning at boot.

`set_value/3` should return `ok` or `{error, Reason}`. Anything other than an error counts as a successful write.

Optionally you can also implement `start_link/0`, which will start a process under nova's supervisor for handling infrastructure of your handler, if it is necessary. For example, in default `nova_session_ets` module it starts a `gen_server` for managing underlying *ETS* table.

```erlang
-module(my_session_backend).
-behaviour(nova_session).

-export([get_value/2, set_value/3, delete_value/1, delete_value/2, session_exists/1]).

get_value(SessionId, Key) ->
    %% Your code here
    {error, not_found}.

set_value(SessionId, Key, Value) ->
    %% Your code here
    ok.

delete_value(SessionId) ->
    %% Your code here
    ok.

delete_value(SessionId, Key) ->
    %% Your code here
    ok.

session_exists(SessionId) ->
    %% Your code here
    false.
```

This module can then be set as the backend in the `sys.config` file under `nova`.

```erlang
{session_manager, my_session_backend}.
```
