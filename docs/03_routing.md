# 03 - Routing

Each nova application have their own routes file. This file contains information about all the routes existing for this application.

## Basic components

A simple route file could look something like this:

```erlang
#{prefix => "/admin",
  security => false,
  routes => [
    {"/", my_controller, main, #{methods => [get]}}
  ]
}
```

This will create a path for `/admin` which, when a user enters will call `my_controller:main/1`.

### The routing object

The routing object consist of four or three fields.

**HTTP Routing**

```
{Route :: list(), Controller :: atom(), Function :: atom(), Options :: map()}
```

**Websocket routing**

```
{Route :: list(), Controller :: atom(), Options :: map()}
```

The websocket controller needs to implement three functions: `websocket_init/1`, `websocket_handle/2`, `websocket_info/2`. More information about these callbacks can be found in the [Cowboy documentation](https://ninenines.eu/docs/en/cowboy/2.6/guide/ws_handlers/).

*Important*
One needs to define `protocol => ws` in the options-map in order to enable websocket communications.


## How to create routes

A route consists of four different components:

*Path* - This is the actual path to the endpoint. Eg "/admin"
*Method* - What method you want to support for this endpoint (get, post, update, delete). If you want to support all methods you can use the `'_'`-atom.
*Controller* - What erlang module should be called on when the path gets called
*Function* - Which function in the *controller* will be called when path gets called.

## Using prefix

You can group paths where you prefix them with a path. This is especially useful when having several different nova applications running. A very common example would be if you had a administration interface. Then the prefix would be `"/admin`". Another example is versioned APIs.

Prefix is defined at top level of a route-entry which can be seen in the *Basic components*-section of this chapter.

## Secure routing

You can secure routes by providing a module and function to the `security` directive in the route entry. Here's a simple example of this:

```erlang
#{prefix => "/admin",
  type => html,
  security => {security_controller, do_security},
  routes => [
    {"/", my_controller, main, #{methods => [get]}}
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

This will cause nova to return a `401` status code for all requests not coming from my_domain.com
