# Testing

Controllers, security callbacks and plugins are functions of a request map,
so most of an application can be tested without a running server. Tests that
go through the router, the plugins and the security callbacks start the
application and send it requests.

A project created with `rebar3 new nova` has a `test` profile that loads
`config/dev_sys.config.src` for Common Test:

```erlang
{test, [
    {ct_opts, [{sys_config, "./config/dev_sys.config.src"}]},
    {extra_src_dirs, [{"test", [{recursive, true}]}]}
]}
```

## Unit tests

Call the function with a map that has the keys it reads:

```erlang
-module(my_app_items_controller_tests).
-include_lib("eunit/include/eunit.hrl").

show_returns_the_id_test() ->
    Req = #{method => ~"GET", bindings => #{~"id" => ~"42"}},
    ?assertEqual({json, #{id => ~"42"}}, my_app_items_controller:show(Req)).

create_echoes_the_item_test() ->
    Req = #{method => ~"POST", json => #{~"name" => ~"book"}},
    ?assertEqual({json, #{created => #{~"name" => ~"book"}}}, my_app_items_controller:create(Req)).
```

The `cowboy_req` functions that only read the request work on such a map
too. `cowboy_req:header/2` reads the `headers` key, a map with lowercase
binary names:

```erlang
bearer_without_a_token_is_401_test() ->
    Req = #{headers => #{}},
    ?assertEqual({false, 401, #{~"www-authenticate" => ~"Bearer"}}, my_app_auth:bearer(Req)).

bearer_with_a_valid_token_test() ->
    Req = #{headers => #{~"authorization" => ~"Bearer letmein"}},
    ?assertEqual({true, #{user_id => ~"7"}}, my_app_auth:bearer(Req)).
```

A plugin is tested the same way, with the options it would get from the
configuration:

```erlang
maintenance_plugin_stops_the_request_test() ->
    Req = #{method => ~"GET"},
    ?assertMatch({stop, {reply, 503, _, _}, Req, state},
                 my_app_maintenance_plugin:pre_request(Req, #{}, #{enabled => true}, state)).
```

Functions that write to the response or use the session, such as
`cowboy_req:set_resp_cookie/4` followed by a reply and `nova_session`, need
a real request. Test those through the server.

Run the tests with `rebar3 eunit`.

## Tests through the server

A Common Test suite that starts the application on a free port and uses the
`httpc` client of OTP:

```erlang
-module(my_app_items_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([show_returns_the_item/1, create_answers_201/1, unknown_path_is_404/1]).

all() ->
    [show_returns_the_item, create_answers_201, unknown_path_is_404].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    Port = free_port(),
    _ = application:load(nova),
    ok = application:set_env(nova, cowboy_configuration, #{port => Port}),
    {ok, _} = application:ensure_all_started(my_app),
    [{base_url, "http://localhost:" ++ integer_to_list(Port)} | Config].

end_per_suite(_Config) ->
    ok = application:stop(my_app),
    ok = application:stop(nova).

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

show_returns_the_item(Config) ->
    {200, Body} = request(get, {?config(base_url, Config) ++ "/api/items/42", []}),
    ?assertEqual(#{~"id" => ~"42"}, json:decode(Body)).

create_answers_201(Config) ->
    Request = {?config(base_url, Config) ++ "/api/items", [], "application/json", ~"{\"name\":\"book\"}"},
    {201, Body} = request(post, Request),
    ?assertEqual(#{~"created" => #{~"name" => ~"book"}}, json:decode(Body)).

unknown_path_is_404(Config) ->
    {404, _Body} = request(get, {?config(base_url, Config) ++ "/nope", []}).

request(Method, Request) ->
    {ok, {{_, Status, _}, _Headers, Body}} =
        httpc:request(Method, Request, [{autoredirect, false}], [{body_format, binary}]),
    {Status, Body}.
```

Run it with `rebar3 ct`.

Notes:

* The rest of the configuration, `bootstrap_application` included, comes from
  the file named in `ct_opts`. The suite only replaces the port, so it does
  not collide with a node that is already running.
* `{autoredirect, false}` makes `httpc` return a redirect instead of following
  it, so a test can assert on the `302` and its `location`.
* `httpc` waits and retries by itself when a response is a `503` with a
  `retry-after` header.
* To send a session, take the `set-cookie` header of the response that
  started it and send its first part back in a `cookie` header.
* To post a form that `nova_csrf_plugin` protects, request the page first.
  Send the `csrf_token` of that page back in the `x-csrf-token` header,
  together with the session cookie of the same response.
* Stop your application and `nova` in `end_per_suite/1`, as above, so the
  listener is gone before the next suite starts its own.

`rebar3 nova gen_test --name <name>` generates a Common Test suite with one
case per route of a controller, see [Rebar3 Nova](rebar3_nova.md).

[nova_test](https://github.com/novaframework/nova_test) is a separate library
with helpers for tests of Nova applications.

## What to cover

* Each route group that has a security callback: a request without
  credentials is refused, one with credentials is let through.
* Each form of a signed-in user: a `POST` without the CSRF token gets a
  `403`, and so does one with the token and `sec-fetch-site: cross-site`.
* Each route group whose security callback reads `json` or `params`: the
  callback finds them, which needs the plugin under
  `plugins_before_security`.
* Each websocket route: an upgrade with the `origin` of another site gets a
  `403`.
* Each status page of your own: `404` for an unknown path, `500` for a
  controller that crashes.
* `rebar3 nova audit --env prod --strict` in CI, to catch a route group that
  lost its `security`.
