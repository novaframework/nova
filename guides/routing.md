# Routing

Each nova application have their own routes file. This file contains information about all the routes existing for this application.

## Basic components

A simple route file could look something like this:

```erlang
-module(my_app_router).

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

You can secure routes by providing a module and function to the `security` directive in the route entry. Here's a simple example of this:

```erlang
#{prefix => "/admin",
  type => html,
  security => fun security_controller:do_security/1,
  routes => [
    {"/", fun my_controller:main/1, #{methods => [get]}}
  ]
}
```

This will cause nova to call `security_controller:do_security/1` before calling the actual controller for all routes defined in the above route entry.
The security function should return a boolean (If the user can proceed or not).


An example of a security function:


```erlang
do_security(Req) ->
    maps:get(host, Req) == "my_domain.com".
```

This will cause nova to return a `401` status code for all requests not coming from `my_domain.com`

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


## Adding routes programatically

You can also add routes programatically by calling `nova_router:add_route/2`. This is useful if you want to add routes dynamically. The spec for it is:

```erlang
%% nova_router:add_route/2 specification
-spec add_route(App :: atom(), Routes :: map() | [map()]) -> ok.
```

First argument is the application you want to add the route to. The second argument is the route or a list of routes you want to add - it uses the same structure as in the regular routers.

```erlang
nova_router:add_route(my_app, #{prefix => "/admin", routes => [{"/", fun my_controller:main/1, #{methods => [get]}}]}).
```

This will add the routes defined in the second argument to the `my_app` application.

**Note**: If a route already exists it will be overwritten.
