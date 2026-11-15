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

This function deletes the whole session and clears the session cookie, which is how a user is signed out. The returned request carries a fresh session id, so a later `set/3` on it in the same request starts a new session instead of reviving the deleted one.

```erlang
logout(Req) ->
    {ok, _SignedOut} = nova_session:delete(Req),
    {redirect, "/"}.
```

### `nova_session:sign_in/3`

```erlang
-spec sign_in(Req :: cowboy_req:req(), Key :: binary(), Value :: term()) ->
          {ok, Req :: cowboy_req:req()} | {error, Reason :: atom()}.
```

Signs a user in. It moves the client to a new session id, stores `Value` under `Key` in the new session and returns the request that carries the new id. Call it where a user signs in, and go on with the request it returns:

```erlang
login(Req) ->
    {ok, SignedIn} = nova_session:sign_in(Req, <<"user_id">>, UserId),
    ok = nova_session:set(SignedIn, <<"signed_in_at">>, erlang:system_time(second)),
    {redirect, "/"}.
```

This is the defence against session fixation. The session the client had is deleted with everything in it, so a session id that somebody else knows, because they planted it in the browser of the user or saw it, is not the id of the user who signed in. Writing who signed in with `set/3` alone keeps the id, and signs in whoever holds it.

`{error, Reason}` is what the session backend answered when it did not store the value. The id has changed by then, and nobody is signed in.

A token of `nova_csrf_plugin` that was made for the old id is refused once the user has signed in, also the one in `csrf_token` of the request that signs in. Answer a sign-in with a redirect, so that the next page carries a token for the new id.

### `nova_session:renew/1`

```erlang
-spec renew(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
```

Moves the client to a new, empty session: the old session's data is deleted and the response carries a new session cookie. It is the lower-level piece of `sign_in/3`, which is what to call where a user signs in, and `delete/1` is what signs out. Call `renew/1` itself for another change of what the client may do, such as a second factor or a password change, and use the returned request for the `set/3` calls that follow.

```erlang
second_factor_passed(Req, UserId) ->
    {ok, Req1} = nova_session:renew(Req),
    ok = nova_session:set(Req1, <<"user_id">>, UserId),
    ok = nova_session:set(Req1, <<"second_factor">>, true),
    {redirect, "/"}.
```

The cookie is written by `nova_stream_h`, so it reaches the client even when the controller does not return the request `sign_in/3`, `renew/1` or `delete/1` gave it. This works for changes made by the process serving the request, which is where controllers, plugins, security functions and `c:nova_websocket:init/1` run (a session written in `init/1` gets its cookie on the `101` response). A process spawned with the request can still read and write session data, but no cookie is sent for what it does.

A `stream_handlers` configuration without `nova_stream_h` keeps storing session data, and `sign_in/3`, `renew/1` and `delete/1` then set or expire the cookie on the request they return, with the same attributes. Return that request, for example with `{json, 200, #{}, Req1, Body}` or `{redirect, Path, Req1}`, or the cookie is lost. A `set/3` from a client without a cookie does not start a session there, and presented ids are used without the check described below.

## The session cookie

The session id is a random 256-bit value from `crypto:strong_rand_bytes/1`, sent in the `__Host-session_id` cookie, or in `session_id` when the `environment` is `dev` (`session_cookie_name` changes the name, see [The `__Host-` cookie name](#the-__host-cookie-name)). `nova_stream_h` issues the cookie with the response of the first request that writes to the session, so visitors who never store anything get no cookie and their responses stay cacheable.

An id the client presents that the backend does not know (never issued, deleted or expired) is ignored and a new one is used, so a client cannot simply choose its own session id. This is defence in depth; `sign_in/3` is what protects against fixation. The check runs in the request process the first time the request uses the session, not in the connection process.

With `secret_key_base` set, the ids Nova makes are signed, and a visitor keeps the id a page handed out when the session is first written. A signed id is the 44 characters Nova handed out and nothing else: the same bytes written another way, with a space in the cookie value or with other bits to spare in the last character, are an id nobody signed, and are replaced like any other unknown id.

A session that was deleted, by `delete/1`, `renew/1` or `sign_in/3`, does not come back. A request that was on its way when another request of the same client signed out, or signed in, and that writes to the session afterwards, is given a session of its own under a new id, and never stores anything under the id the client was moved off. The default `nova_session_ets` backend remembers a deleted id for five minutes, on every node, for this: a write to it in that time stores nothing and returns `{error, session_ended}` from the backend. It remembers at most as many ended ids as it may hold sessions (`max_sessions`, or 1 000 000 when that is `infinity`), the oldest going first. A request that takes longer than five minutes is not covered.

A request that carries two or more different values for the session cookie, for example because a cookie of the same name was set for a parent domain or another path, is treated as having no session, and a warning is logged once per node.

The cookie is sent with `Path=/`, `HttpOnly`, `SameSite=Lax`, `Max-Age` set to `session_max_age`, and with `Secure` unless `{nova, [{environment, dev}]}` is set (a dev request over HTTPS still gets `Secure`). An unset `environment` counts as not `dev` here, although `nova:get_environment/0` returns `dev` for it. Behind a proxy that terminates TLS Nova sees plain HTTP, which is why `Secure` does not depend on the scheme. A staging environment served over plain HTTP that is not marked `dev` gets a cookie the browser refuses to store, so sessions do not work there until you serve it over HTTPS, or name the cookie without the `__Host-` prefix and set `secure => false` in `session_cookie_opts`:

```erlang
{nova, [
    {session_cookie_name, <<"session_id">>},
    {session_cookie_opts, #{secure => false}},
    {session_max_age, 86400}
]}
```

Nova cannot see whether a proxy in front of it has the TLS. So when it starts outside `dev` with a listener that has no TLS and a session cookie that is `Secure`, it logs one notice (`notice => session_cookie_needs_https`) that says what the cookie needs and names these settings. Behind a proxy that terminates TLS there is nothing to change.

The same applies to local development. Over plain HTTP without `{nova, [{environment, dev}]}`, the browser refuses to store the `Secure` session cookie Nova sets (some browsers make an exception for `localhost`). Every request then starts a new, empty session, and signing in seems to do nothing. Set the environment in your development config:

```erlang
{nova, [{environment, dev}]}
```

### The `__Host-` cookie name

A cookie named with the `__Host-` prefix is only accepted by browsers when it is `Secure`, has `Path=/` and no `Domain`, so a sibling or parent domain cannot set or shadow it. When `session_cookie_name` is not set, the session cookie is named `__Host-session_id`, and `session_id` when the `environment` is `dev`, where the site is reached over plain HTTP and a browser would not keep the other.

With a `__Host-` name Nova always sends `Secure` and `Path=/` and drops `domain`, whatever `session_cookie_opts` says, and `Secure` does not depend on the listener: behind a proxy that terminates TLS the browser sees https, and keeps the cookie.

An application that needs what the prefix rules out, a cookie for several hosts of a domain (`domain`), for a part of the site (`path`) or one sent over plain HTTP (`secure => false`), names its cookie:

```erlang
{nova, [
    {session_cookie_name, <<"session_id">>},
    {session_cookie_opts, #{domain => <<"example.com">>}}
]}
```

With one of those three in `session_cookie_opts` and no `session_cookie_name`, outside `dev`, Nova does not start: `{invalid_setting, session_cookie_opts, {requires, session_cookie_name}}`. It would otherwise send a cookie without what was asked for.

Changing the name of the cookie signs every client out once, and so does an upgrade from a Nova that named it `session_id`: the old cookie is no longer read, so clients get a new session under the new name. The old cookie stays in the browser until it expires. To keep the sessions there are, set `{session_cookie_name, <<"session_id">>}`.

`session_cookie_opts` takes the options of `cowboy_req:set_resp_cookie/4` (`path`, `domain`, `secure`, `http_only`, `same_site`, `max_age`) and is merged over the defaults above. Other keys, and values of the wrong type (a `path` that is not a binary, for example), are ignored. The cookie name must be an RFC 6265 token and `path` and `domain` plain ASCII without control characters or `;`; Nova checks both at boot and refuses to start otherwise, as it does for a name that is empty or not a binary. Changed at runtime to something invalid, `nova_stream_h` (one of the default `stream_handlers`) leaves the cookie off the response and logs one error per cookie name. Without `nova_stream_h`, `renew/1` and `delete/1` set the cookie themselves, and an invalid setting makes them raise, so the request gets a 500.

`session_max_age` (seconds, 14 days by default) limits how long a session lives from the moment it was created, whatever happens in between. It sets `Max-Age` on the cookie, and the default `nova_session_ets` backend drops the session once it has passed. With `infinity` the cookie has no `Max-Age` and sessions live until they are deleted or the node restarts.

`max_sessions` (1 000 000 by default) limits how many sessions the default `nova_session_ets` backend holds. A request without a session cookie that writes to the session starts a new one, and `nova_csrf_plugin` does that on every such request, so without a limit any client could fill the memory of every node. When the table is full, expired sessions are dropped first. If that leaves no room, `nova_session:set/3` returns `{error, session_store_full}` for a session that does not exist yet and a warning is logged, at most once a minute. Sessions that exist are read and written as before. `infinity` turns the limit off.

The default `nova_session_ets` backend keeps sessions in memory, replicated to the nodes connected at the time of the write. They do not survive a restart, and a node that joins later does not have the sessions written before it joined. Because unknown ids are replaced, a client that reaches such a node gets a fresh, empty session and is signed out. That happens:

- behind a load balancer without sticky sessions;
- during a rolling deploy;
- after a restart.

To run several nodes without sticky sessions, or to keep sessions across restarts, use a shared session store: a `nova_session` backend over a database or cache, set with `session_manager` (see [Backend](#backend)).

## Enabling Sessions

Sessions are enabled by default in Nova, but if you need to disable them, you can do so by setting the `use_sessions` configuration option to `false`. This will disable sessions for the entire site. Like `json_lib`, it is read from your bootstrap application's environment first and then from `nova`'s (see the [configuration guide](configuration.md)).

It's also possible to set the backend for the session data. By default, the session data is stored in an *ETS* table in the Erlang VM but it's quite easy to implement a custom backend.

## Backend

You can implement your own backend by creating a module that implements the `nova_session` behaviour. This behaviour has four functions that need to be implemented: `get_value/2`, `set_value/3`, `delete_value/1` and `delete_value/2`.

Implement the optional `session_exists/1` too. Nova calls it for the session id a client presents and replaces ids it returns `false` for. It is called on every `get/2`, `set/3` and `delete` of a request that presented a cookie, so keep it as cheap as a key lookup. A backend without it accepts any id a client sends, and Nova logs a warning at boot.

`set_value/3` should return `ok` or `{error, Reason}`. Anything other than an error counts as a successful write.

Implement the optional `session_ended/1` as well when the backend can: it tells whether the session with an id was deleted with `delete_value/1` a short while ago, for at least as long as a request to your application can take. With signed ids (`secret_key_base`), Nova starts a session under a signed id that no session is stored under, and it asks `session_ended/1` first, so that a request that was on its way when its client signed out does not start one under the id that was given up. A backend that has it should also store nothing under such an id and return `{error, session_ended}` from `set_value/3`, since the session can be deleted between the moment Nova asks `session_exists/1` and the write; Nova then stores the value in a session of its own. A row with the id and a time to live of a few minutes, written by `delete_value/1`, is enough. A backend without the callback relies on what the process of the request knows: a session it has read, deleted or renewed itself is not written again, and one that another request ended before this one touched it can be.

Optionally you can also implement `start_link/0`, which will start a process under nova's supervisor for handling infrastructure of your handler, if it is necessary. For example, in default `nova_session_ets` module it starts a `gen_server` for managing underlying *ETS* table.

```erlang
-module(my_session_backend).
-behaviour(nova_session).

-export([get_value/2, set_value/3, delete_value/1, delete_value/2, session_exists/1, session_ended/1]).

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

session_ended(SessionId) ->
    %% Your code here: true for an id delete_value/1 was called with a few minutes ago
    false.
```

This module can then be set as the backend in the `sys.config` file under `nova`.

```erlang
{session_manager, my_session_backend}.
```
