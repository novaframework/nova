# Sessions

Sessions in Nova are handled by the `nova_session` module. It stores values for a client between requests and takes care of the session id and its cookie.

Sessions are on by default. With `{use_sessions, false}` every function of `nova_session` throws `{nova_session, unsupported_session_used}`, which a controller that does not catch it answers with a `500`.

## API

### `nova_session:set/3`

```erlang
-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: term()) ->
                 ok | {error, Reason :: atom()} | no_return().
```

Sets a session value. It takes three arguments: the request, the key and the value. Keys are binaries. The value is any term: it is handed to the session backend as it is, and `nova_session_ets` returns it unchanged from `get/2`. A backend of your own decides what it can store.

```erlang
ok = nova_session:set(Req, ~"name", ~"John Doe").
```

Use it for what a signed-in client stores. For a visitor who is not signed in, use `set_provisional/3`.

### `nova_session:set_provisional/3`

```erlang
-spec set_provisional(Req :: cowboy_req:req(), Key :: binary(), Value :: term()) ->
          ok | {error, Reason :: atom()} | no_return().
```

Sets a session value like `set/3`, for a value that any visitor gets and that can be made again, such as a flash message, a cart or a CSRF token. See [Data for visitors who are not signed in](#data-for-visitors-who-are-not-signed-in).

```erlang
ok = nova_session:set_provisional(Req, ~"flash", ~"Saved").
```

### `nova_session:get/2`

```erlang
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
          {ok, Value :: term()} | {error, Reason :: atom()} | no_return().
```

Gets a session value. It takes two arguments: the request and the key. It returns `{error, not_found}` when the client has no session or the session has no such key.

```erlang
{ok, Name} = nova_session:get(Req, ~"name").
```

### `nova_session:delete/2`

```erlang
-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} | no_return().
```

Deletes a value. It takes two arguments: the request and the key. The rest of the session is kept.

```erlang
{ok, Req1} = nova_session:delete(Req, ~"name").
```

### `nova_session:delete/1`

```erlang
-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} | no_return().
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
          {ok, Req :: cowboy_req:req()} | {error, Reason :: atom()} | no_return().
```

Signs a user in. It moves the client to a new session id, stores `Value` under `Key` in the new session and returns the request that carries the new id. Call it where a user signs in, and go on with the request it returns:

```erlang
login(Req) ->
    {ok, SignedIn} = nova_session:sign_in(Req, ~"user_id", UserId),
    ok = nova_session:set(SignedIn, ~"signed_in_at", erlang:system_time(second)),
    {redirect, "/"}.
```

This is the defence against session fixation. The session the client had is deleted with everything in it, so a session id that somebody else knows, because they planted it in the browser of the user or saw it, is not the id of the user who signed in. Writing who signed in with `set/3` alone keeps the id, and signs in whoever holds it. Copy what is to survive the sign-in, such as a cart, to the request `sign_in/3` returns.

`{error, Reason}` is what the session backend answered when it did not store the value. The id has changed by then, and nobody is signed in.

A token of `nova_csrf_plugin` that was made for the old id is refused once the user has signed in, also the one in `csrf_token` of the request that signs in. Answer a sign-in with a redirect, so that the next page carries a token for the new id.

### `nova_session:renew/1`

```erlang
-spec renew(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} | no_return().
```

Moves the client to a new, empty session: the old session's data is deleted and the response carries a new session cookie. It is the lower-level piece of `sign_in/3`, which is what to call where a user signs in, and `delete/1` is what signs out. Call `renew/1` itself for another change of what the client may do, such as a second factor or a password change, and use the returned request for the `set/3` calls that follow.

```erlang
second_factor_passed(Req, UserId) ->
    {ok, Renewed} = nova_session:renew(Req),
    ok = nova_session:set(Renewed, ~"user_id", UserId),
    ok = nova_session:set(Renewed, ~"second_factor", true),
    {redirect, "/"}.
```

### `nova_session:client_id/1`

```erlang
-spec client_id(Req :: cowboy_req:req()) -> {ok, SessionId :: binary()} | {error, not_found} | no_return().
```

Returns the session id the client of the request holds. It stores nothing and starts no session.

```erlang
{ok, ClientId} = nova_session:client_id(Req).
```

| The request | Returns | The response |
|---|---|---|
| Has a session cookie | `{ok, Id}` with the id in the cookie, whether or not there is a session for it | Sets no cookie |
| Has no session cookie | `{ok, Id}` with the id of the request | Hands that id out in the session cookie |
| Has a `Cookie` header that does not parse | `{ok, Id}` with the id of the request | Sets no cookie, since the client may hold one Nova could not read. The id is not seen again |
| Has no session cookie, and `nova_stream_h` is not a stream handler | `{error, not_found}` | Sets no cookie |

It is for what is tied to the browser and not to what a session holds. `nova_csrf_plugin` signs its tokens for this id, see [CSRF token modes](plugins.md#csrf-token-modes). With `secret_key_base` set the id is signed, and the first `set/3` stores the session under it, so the cookie stays as it is. Without a secret the id is not the one `set/3` writes under when the backend has no session for it: the first `set/3` then moves the client to a new id, and the response replaces the cookie.

### The cookie and the request that is returned

The cookie is written by `nova_stream_h`, so it reaches the client even when the controller does not return the request that `sign_in/3`, `renew/1` or `delete/1` gave it. This works for changes made by the process serving the request, which is where controllers, plugins, security functions and `c:nova_websocket:init/1` run (a session written in `init/1` gets its cookie on the `101` response). A process spawned with the request can still read and write session data, but no cookie is sent for what it does.

A `stream_handlers` configuration without `nova_stream_h` keeps storing session data, and `sign_in/3`, `renew/1` and `delete/1` then set or expire the cookie on the request they return, with the same attributes. Return that request, for example with `{json, 200, #{}, Body, Req1}` or `{redirect, Path, Req1}`, or the cookie is lost. A `set/3` from a client without a cookie does not start a session there, and presented ids are used without the check described below.

## The session cookie

The session id is 32 bytes in base64, 44 characters, sent in the `session_id` cookie (`session_cookie_name` changes the name). Without `secret_key_base` all 32 bytes come from `crypto:strong_rand_bytes/1`. With it, 16 bytes do, and the other 16 are their HMAC-SHA-256 under the key Nova has from the secret for session ids. `nova_stream_h` issues the cookie with the response of the first request that writes to the session, so visitors who never store anything get no cookie and their responses stay cacheable. `nova_csrf_plugin` hands the cookie out as well when it signs its tokens, without starting a session.

An id the client presents that the backend does not know (never issued, deleted or expired) is ignored and a new one is used, so a client cannot simply choose its own session id. This is defence in depth; `sign_in/3` is what protects against fixation. The check runs in the request process the first time the request uses the session, not in the connection process.

With `secret_key_base` set, the ids Nova makes are signed, so Nova knows an id it handed out without having stored anything for it. A visitor then keeps the id a page handed out when the session is first written: the cookie stays as it is, and so do the tokens `nova_csrf_plugin` signed for it. An id that no secret in use signed is replaced by the first write, as it is without a secret. A signed id is the 44 characters Nova handed out and nothing else: the same bytes written another way, with a space in the cookie value or with other bits to spare in the last character, are an id nobody signed, and are replaced like any other unknown id.

Because a signed id is kept, the id of a client changes with `sign_in/3`, `renew/1` and `delete/1` and with nothing else. An application that writes who signed in with `set/3` alone signs in whoever holds the id.

A session that was deleted, by `delete/1`, `renew/1` or `sign_in/3`, does not come back. A request that was on its way when another request of the same client signed out, or signed in, and that writes to the session afterwards, is given a session of its own under a new id, and never stores anything under the id the client was moved off. The default `nova_session_ets` backend remembers a deleted id for five minutes, on every node, for this: a write to it in that time stores nothing and returns `{error, session_ended}` from the backend. It remembers at most as many ended ids as it may hold sessions (`max_sessions`, or 1 000 000 when that is `infinity`), the oldest going first. A request that takes longer than five minutes is not covered.

A request that carries two or more different values for the session cookie, for example because a cookie of the same name was set for a parent domain or another path, is treated as having no session, and a warning is logged once per node.

The cookie is sent with `Path=/`, `HttpOnly`, `SameSite=Lax`, `Max-Age` set to `session_max_age`, and with `Secure` unless `{nova, [{environment, dev}]}` is set (a dev request over HTTPS still gets `Secure`). An unset `environment` is read as `prod`, so the cookie is `Secure` then. Behind a proxy that terminates TLS Nova sees plain HTTP, which is why `Secure` does not depend on the scheme. A staging environment served over plain HTTP that is not marked `dev` gets a cookie the browser refuses to store, so sessions do not work there until you set `secure => false` in `session_cookie_opts` or serve it over HTTPS:

```erlang
{nova, [
    {session_cookie_opts, #{secure => false}},
    {session_max_age, 86400}
]}
```

The same applies to local development. Over plain HTTP without `{nova, [{environment, dev}]}`, the browser refuses to store the `Secure` session cookie Nova sets (some browsers make an exception for `localhost`). Every request then starts a new, empty session, and signing in seems to do nothing. Set the environment in your development config:

```erlang
{nova, [{environment, dev}]}
```

### The `__Host-` cookie name

A cookie named with the `__Host-` prefix is only accepted by browsers when it is `Secure`, has `Path=/` and no `Domain`, so a sibling or parent domain cannot set or shadow it. New applications should use it:

```erlang
{nova, [{session_cookie_name, <<"__Host-session_id">>}]}
```

With a `__Host-` name Nova always sends `Secure` and `Path=/` and drops `domain`, whatever `session_cookie_opts` says. Changing the name of an existing application signs every client out once: the old `session_id` cookie is no longer read, so clients get a new session under the new name. The old cookie stays in the browser until it expires.

`session_cookie_opts` takes the options of `cowboy_req:set_resp_cookie/4` (`path`, `domain`, `secure`, `http_only`, `same_site`, `max_age`) and is merged over the defaults above. Other keys, and values of the wrong type (a `path` that is not a binary, for example), are ignored. The cookie name must be an RFC 6265 token and `path` and `domain` plain ASCII without control characters or `;`; Nova checks both at boot and refuses to start otherwise, as it does for a name that is empty or not a binary. Changed at runtime to something invalid, `nova_stream_h` (one of the default `stream_handlers`) leaves the cookie off the response and logs one error per cookie name. Without `nova_stream_h`, `sign_in/3`, `renew/1` and `delete/1` set the cookie themselves, and an invalid setting makes them raise, so the request gets a 500.

`session_max_age` (seconds, 14 days by default) limits how long a session lives from the moment it was created, whatever happens in between. It sets `Max-Age` on the cookie, and the default `nova_session_ets` backend drops the session once it has passed. With `infinity` the cookie has no `Max-Age` and sessions live until they are deleted or the node restarts.

The default `nova_session_ets` backend keeps sessions in memory, replicated to the nodes connected at the time of the write. They do not survive a restart, and a node that joins later does not have the sessions written before it joined. Because unknown ids are replaced, a client that reaches such a node gets a fresh, empty session and is signed out. That happens:

- behind a load balancer without sticky sessions;
- during a rolling deploy;
- after a restart.

To run several nodes without sticky sessions, or to keep sessions across restarts, use a shared session store: a `nova_session` backend over a database or cache, set with `session_manager` (see [Backend](#backend)).

## Data for visitors who are not signed in

A request without a session cookie that writes to the session starts a new one. Anyone can send such requests, so sessions that are started for visitors who are not signed in must not be able to push out the sessions of users who are. `nova_session` has two ways to write for that reason:

| Written with | The session is | Lives | In a full store |
|---|---|---|---|
| `nova_session:set_provisional/3` only | provisional | One day, or `session_max_age` when that is shorter | Dropped first. A new one only takes the place of an expired or another provisional session |
| `nova_session:set/3`, once or more | in use | `session_max_age` | Dropped only for another session in use, when there is no expired and no provisional session left |

The lifetimes and the order are those of the default backend, `nova_session_ets`. A [backend](#backend) of your own decides what it does with the two kinds of write.

Store what a visitor gets before signing in with `set_provisional/3`: a flash message, a cart, a language choice. `nova_csrf_plugin` stores its token that way when it keeps tokens in the session, see [CSRF token modes](plugins.md#csrf-token-modes).

```erlang
add_to_cart(#{bindings := #{~"id" := Id}} = Req) ->
    Cart = case nova_session:get(Req, ~"cart") of
               {ok, Items} -> Items;
               {error, _} -> []
           end,
    case nova_session:set_provisional(Req, ~"cart", [Id | Cart]) of
        ok -> {redirect, "/cart"};
        {error, _} -> {status, 503}
    end.
```

The first `set/3` on a provisional session makes it a session in use, with the lifetime of a session created at that point. The values written before stay. A `set_provisional/3` on a session in use writes the value and leaves the session in use. `sign_in/3` writes with `set/3`, so a client that signs in has a session in use. It starts an empty session, so copy what is to survive the sign-in, such as the cart, to the request it returns.

An application that calls `set/3` for visitors who are not signed in starts a session in use for each of them. When the store is full, a flood of requests without a cookie to such a route drops the oldest sessions in use, and signs users out.

## When the session store is full

`max_sessions` (1 000 000 by default) limits how many sessions the default `nova_session_ets` backend holds on a node. Without a limit any client could fill the memory of every node. In a full table a write that starts a session drops another one to make room:

1. a session that has expired,
2. a provisional session, the one nearest its expiry first,
3. for a `set/3` only, the session in use that is nearest its expiry.

A `set_provisional/3` that would start a session returns `{error, session_store_full}` when the table holds nothing but sessions in use, and no cookie is issued. `set/3` returns it when the session is not in the table of the node after the write, which happens when a session from another node took the room that was made for it. Sessions that exist are read and written as before. A warning is logged when a session that has not expired is dropped or a write is refused, at most once a minute.

`max_sessions` is an integer above 0, or `infinity` for no limit. It is read from the `nova` environment when Nova starts. Any other value, `0` included, stops the boot with `{invalid_setting, max_sessions, Value}`, see [Configuration](configuration.md#settings-checked-at-boot).

```erlang
{nova, [{max_sessions, 200000}]}
```

Every node keeps to the limit for the sessions the other nodes send it as well, and each node chooses what it drops on its own, so under pressure the nodes do not hold the same sessions.

What a flood of requests without a cookie can still do with these rules is listed in the [Security](security.md#when-the-session-store-is-full) guide.

## Enabling Sessions

Sessions are enabled by default in Nova, but if you need to disable them, you can do so by setting the `use_sessions` configuration option to `false`. This will disable sessions for the entire site. Like `json_lib`, it is read from your bootstrap application's environment first and then from `nova`'s (see the [configuration guide](configuration.md)).

It's also possible to set the backend for the session data. By default, the session data is stored in an *ETS* table in the Erlang VM but it's quite easy to implement a custom backend.

## Backend

You can implement your own backend by creating a module that implements the `nova_session` behaviour. This behaviour has four functions that need to be implemented: `get_value/2`, `set_value/3`, `delete_value/1` and `delete_value/2`.

Implement the optional `session_exists/1` too. Nova calls it for the session id a client presents and replaces ids it returns `false` for. The answer is kept for one second in the process that calls `nova_session`, for reads: a request that presented a cookie asks once however often it reads its session, and again for every write and after it deleted or renewed it. A backend without it accepts any id a client sends, and Nova logs a warning at boot.

`set_value/3` should return `ok` or `{error, Reason}`. Anything other than an error counts as a successful write. `{error, session_ended}` has a meaning of its own, see `session_ended/1` below. The value is any term, as it was given to `nova_session:set/3`, and `get_value/2` has to return it as it was.

The optional `set_provisional_value/3` gets what is written with `nova_session:set_provisional/3`. A session that only holds such values is not in use yet, so the backend can keep it for a shorter time and drop it first when it runs out of room. A backend without the callback gets those values through `set_value/3`, and then has no way to tell the sessions of visitors from those of signed-in users.

Implement the optional `session_ended/1` as well when the backend can: it tells whether the session with an id was deleted with `delete_value/1` a short while ago, for at least as long as a request to your application can take. With signed ids (`secret_key_base`), Nova starts a session under a signed id that no session is stored under, and it asks `session_ended/1` first, so that a request that was on its way when its client signed out does not start one under the id that was given up. A backend that has it should also store nothing under such an id and return `{error, session_ended}` from `set_value/3` and `set_provisional_value/3`, since the session can be deleted between the moment Nova asks `session_exists/1` and the write; Nova then stores the value in a session of its own. A row with the id and a time to live of a few minutes, written by `delete_value/1`, is enough. A backend without the callback relies on what the process of the request knows: a session it has read, deleted or renewed itself is not written again, and one that another request ended before this one touched it can be.

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
