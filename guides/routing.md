# Routing

Each nova application have their own routes file. This file contains information about all the routes existing for this application.

## Basic components

A simple route file could look something like this:

```erlang
-module(my_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
  [#{prefix => "/admin",
    security => false,
    routes => [
      {"/", fun my_controller:main/1, #{methods => [get]}}
    ]
  }].
```

This will create a path for `/admin` which, when a user enters will call `my_controller:main/1`. The `_Environment` variable that is consumed by the `routes` function will have the value that `nova:get_environment/0` returns. The *environment* variable is an important thing cause it enables the devlopers to define different routes for different environments. To change the running environment edit the `sys.config` file to include an `{environment, Env}` tuple under the `nova` application where `Env` can be any erlang-term.

You can also defer the routing information to the controllers, like this:
```erlang
-module(my_app_router).
-behaviour(nova_router).

-export([controllers/1]).

controllers(_Environment) ->
  [my_controller,
   {my_file_controller, #{root => "/pub"}},
   {my_file_controller, #{root => "/internal"}}
  ].
```

The controller modules then need to provide the `routes/2` callback, like
this (note the extra `Options` argument):

```erlang
-module(my_controller).
-behaviour(nova_controller).

-export([routes/2]).

routes(_Environment, _Options) ->
  [#{prefix => "/admin",
    security => false,
    routes => [
      {"/", fun ?MODULE:main/1, #{methods => [get]}}
    ]
  }].
```

If a controller module is listed as `{Module, Options}`, then `Options`
will be passed as the second argument to `routes/2`, so you can have
multiple uses of the same controller module with different configurations.
If a controller is listed by name only, the options will be an empty map
`#{}`.

These methods are not exclusive - it is possible to specify routes both in
the main router module and in the controllers.

### The routing object

The routing object consists of three or four fields.

### HTTP(S) Routing ###

```
{Route :: list(), ControllerCallback :: function(), Options :: map()}
```

As you saw in the initial example in the [Basic components](#basic-components) section, we defined a route for the root path `/`.

### Websocket routing ###

```
{Route :: list(), Controller :: atom(), Options :: map()}
```

> #### Important {: .tip}
>
> One needs to define `protocol => ws` in the options-map in order to enable websocket communications.


### Static files

One can also define static files to be served by nova. This is done by adding a tuple to the route entries. The value of this tuple should be of size 2 or  3  where the first element is the url and the second element is the path to the file on the filesystem related from the apps `priv` directory. An additional third element can be added to the tuple to define options for this particular static file or directory.

*Note*! The notation `[...]` can be used as a wildcard (Zero or many occurences) in the url-section.
Valid options is;

- `mimetype` - which mimetype the file should be served as. If not defined nova will try to guess the mimetype based on the file extension.
- `index_files` - a list of filenames that can be used as an index. This is relevant if a directory is served.
- `list_dir` - Set to true if allowing the requester to list the content of a directory (if such is served)

Example:
```erlang
{"/my/static/directory/[...]", "assets/a-local-dir", #{list_dir => true}},
{"/with-index/[...]", "assets/another-dir", #{index_files => ["index.html"]}}
```

## How to create routes

A route consists of three different components:

* `Path` - This is the actual path to the endpoint. Eg `"/admin"`
* `Method` - What method you want to support for this endpoint (get, post, update, delete). If you want to support all methods you can use the `'_'`-atom.
* `ControllerCallback` - What erlang callback should be called on when the path gets called. This is defined as a function reference, eg `fun my_controller:main/1`.

## Using prefix

You can group paths where you prefix them with a path. This is especially useful when having several different nova applications running. A very common example would be if you had an administration interface. Then the prefix would be `"/admin"`. Another example is versioned APIs.

Prefix is defined at top level of a route-entry which can be seen in the [Basic components](#basic-components) section of this chapter.

## Secure routing

When building web applications, ensuring the security of your routing layer is critical. Nova Framework’s routing system is designed with flexibility and security in mind, but developers must implement best practices to protect their applications from common vulnerabilities. This section outlines key considerations and features of Nova's routing system to help you achieve secure routing.

### Invoking security functions for a set of endpoints

You can define a security function that will be called before the actual controller is called. This is useful if you want to check if the user is allowed to access the endpoint. The security function should return a boolean value (Or a special one - more about that later in this section). If the function returns `false` the request will be stopped and a `401` status code will be returned.

The following code will invoke `security_controller:do_security/1` before calling the actual controller to do the security check.

```erlang
-module(my_example_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
    [#{prefix => "/admin",
        security => fun security_controller:do_security/1,
        routes => [
            {"/", fun my_controller:main/1, #{methods => [get]}}
        ]
    }].
```

### The security-function callback

The most simple way to implement a security function is to return a boolean value. If the function returns `false` the request will be stopped and a `401` status code will be returned. There's also an additional return value that can be used to store data that will be available to the controller. This is particularly useful if you want to pass data like user information, roles, etc. to the controller.

#### Return values for the secure function

| **Return value** | **Description** |
| ---------------- | --------------- |
| `true` | The request will continue to the controller. |
| `{true, Data :: term()}` | The request will continue to the controller and the data will be available in the `Req` object under the `auth_data` key. |
| `{redirect, Url :: binary()}` | The request will be redirected to the specified URL with HTTP status 302. |
| `false` | The request will be stopped and a `401` status code will be returned. |
| `{false, Headers :: map()}` | Same as above but also adds additional headers to the response. |
| `{false, StatusCode :: integer(), Headers :: map()}` | Same as above but uses a custom status code. |
| `{false, StatusCode :: integer(), Headers :: map(), Body :: iodata()}` | Same as above but also adds a custom body to the response. |

*Note* If `false` is returned in any form the request will be stopped from executing the controller.


```erlang
-module(security_controller).
-export([do_security/1]).

do_security(Req) ->
    case get_user(Req) of
        {ok, User} ->
            {true, #{user => User}};
        _ ->
            false
    end.

get_user(Req) ->
    ## Do some validatation and return the user
    {ok, #{name => "John Doe", role => "Admin"}}.
```

Once the security function has been called, the data will be available in the `Req` object under the `auth_data` key.

```erlang
-module(my_controller).
-export([main/1]).

main(Req = #{auth_data := User}) ->
    io:format("User: ~p~n", [User]),
    {ok, Req, <<"Hello world!">>}.
```

## Using plugins local to a set of endpoints

It's possible to configure a small set of endpoints with a specific plugin. This is done by adding a `plugins` key to the route entry. The value of this key should be a list of plugins to use for this route entry.

```erlang
#{prefix => "/admin",
  plugins => [
    {pre_request, nova_json_schemas, #{render_errors => true}}
  ],
  routes => [
    {"/", fun my_controller:main/1, #{methods => [get]}}
  ]
}
```

In the example above we have enabled the *pre-request*-plugin `nova_json_schemas` for all routes under the `/admin` prefix. This will cause all requests to be validated against the JSON schema defined in the `nova_json_schemas` plugin.
You can also include *post-request*-plugins in the same way.

By default a route entry that declares `plugins` uses exactly those, and one
that does not uses the plugins configured globally. Set `plugin_strategy` on
the entry to combine them instead:

| Value | Plugins used |
|---|---|
| `local_or_global` | The entry's own plugins if it declares any, otherwise the global ones. This is the default. |
| `local_first` | The entry's own plugins, then the global ones. |
| `global_first` | The global plugins, then the entry's own. |
| `local_only` | Only the entry's own plugins. An entry with none gets none. |
| `global_only` | Only the global plugins. |
| `{override, List}` | Exactly `List`. |

When both sets are combined, a plugin appearing in both is kept once, at its
first position, so ordering within a phase is preserved.

### Overriding security for an included application

When you include another Nova application, you may want to put your own
security callback in front of its routes rather than the one it declares for
itself. Set `override_secure` in the options for that application:

```erlang
{nova_apps, [{another_nova_app, #{prefix => "/admin",
                                  override_secure => fun my_security:check/1}}]}
```

It takes the same values as `secure`: `false` for no override, a `fun/1`, or
the deprecated `{Module, Function}`.


## Route precedence

More than one route can match a request. Nova resolves that the same way every
time, at each path segment in turn:

1. A literal segment.
2. A binding, `:name`. When several bindings sit at the same depth they are
   tried in name order.
3. A `[...]` catch-all.

Matching backtracks, so a route is only skipped if nothing below it can match.
`/users/new` and `/users/:id` can therefore both be declared: `/users/new`
serves the literal path and `/users/:id` serves everything else.

Once a path matches, the method is resolved. An exact method wins over a route
declared with `'_'`. If the path matches but the method does not, Nova answers
`405` with an `allow` header listing the methods that path does accept.

If two routes declare the same path *and* the same method, the first one
registered wins and the second is logged and ignored. Set
`use_strict_routing` to `true` in the `nova` application environment to make
Nova refuse to start on a conflict instead, which also reports overlapping
literal and binding routes.

## Adding routes programatically

You can also add routes programatically by calling `nova_router:add_routes/2`. This is useful if you want to add routes dynamically. The spec for it is:

```erlang
%% nova_router:add_routes/2 specification
-spec add_routes(App :: atom(), Routes :: map() | [map()]) -> ok.
```

First argument is the application you want to add the route to. The second argument is the route or a list of routes you want to add - it uses the same structure as in the regular routers.

```erlang
nova_router:add_routes(my_app, #{prefix => "/admin", routes => [{"/", fun my_controller:main/1, #{methods => [get]}}]}).
```

This will add the routes defined in the second argument to the `my_app` application.

**Note**: If a route already exists it will be overwritten. This is the one
place where a later route wins; routes compiled at startup keep the first.

If the application has a router module you can leave the routes out and let
Nova call it for you:

```erlang
nova_router:add_routes(my_app).
```

To take an application's routes back out again:

```erlang
nova_router:remove_application(my_app).
```

Both work on the routing table of the default listener. If you started the
application on its own listener with `nova_sup:add_application/2`, use
`nova_sup:remove_application/1` instead, which also stops the listener once
nothing is left on it. See the [multi-app guide](multi-app.md).
