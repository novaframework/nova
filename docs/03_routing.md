# 03 - Routing

Each nova application have their own routes file. This file contains information about all the routes existing for this application.

## Basic components

A simple route file could look something like this:

```erlang
#{prefix => "/admin",
  type => html,
  security => false,
  routes => [
    {"/", get, my_controller, main}
  ]
}
```

This will create a path for `/admin` which, when a user enters will call `my_controller:main/1`. You can also choose different types of controllers (`html`, `rest` and `websockets`) and add security to your endpoints.

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
    {"/", get, my_controller, main}
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
