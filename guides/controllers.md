# Controllers

Controllers are central in how Nova works. They are in charge of handling all user-implemented logic for a request.
Controllers is located in `/src/controllers/` of your Nova application. A controller is basically a regular Erlang module but that exposes functions you've provided in the routing file.

![Request life-cycle](images/controller_flow.png "Request life-cycle")

## Handlers

Handlers is modules that interprets the resulting output from a controller and sends it to the requester. Handlers are identified by the first atom in the return of a controller, eg `{json, #{status => "ok"}}` calls the `json`-handler. You can read about the handlers below and what their function are.

### JSON structures

#### Simple interface

*Keyword*: `json`

*Spec*: `{json, Structure :: map()}`

*Example*: `{json, #{status => "ok"}}.`

Converts the map (Second element in the resulting tuple) and sends it to the requester with *HTTP-status code 200*.

#### Advanced interface

*Keyword*: `json`

*Spec*: `{json, StatusCode :: integer(), Headers :: map(), JSON :: map()}`

*Example*: `{json, 201, #{"x-correlation-id", "EX123"}, #{some_response => true}}`

Same as the simple interface but with two additional elements for specifying HTTP-status code and additional headers.

### HTML templates (Using Erlydtl)

*Nova* renders templates using ErlyDTL ([https://github.com/erlydtl/erlydtl](https://github.com/erlydtl/erlydtl) which uses the *django template language* to express logic. To get a better overview for the functionality that comes with dtl check their [documentation page](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html)

#### Simple interface

*Keyword*: `view`

*Spec*: `{view, Variables :: map() | [{Key :: atom() | binary() | string(), Value :: any()}]}`

*Example*: `{view, #{my_var => "123"}}`

Renders the corresponding view with the variables attached. If a controller is named `my_simple_controller.erl` the view is named `my_simple_view.dtl`.

#### Advanced interface

*Keyword*: `view`

*Spec*: `{view, Variables :: map() | [{Key :: atom() | binary() | string(), Value :: any()}], Options :: map()}`

*Example*: `{view, #{my_var => "123"}, #{view => my_view_mod}}`

Same as the simple interface but where you can define some options. Currently the only option for this interface is *view* which enables the user to specify a view other than the corresponding one based on controllers name. In the example above the dtl-file `my_view_mod.dtl` would be rendered.

### HTTP-status codes

#### Simple interface

*Keyword*: `status`

*Spec*: `{status, StatusCode :: integer()}`

*Example*: `{status, 200}`

Returns a HTTP-code to the requester with an empty body.

#### Medium interface

*Keyword*: `status`

*Spec*: `{status, StatusCode :: integer(), ExtraHeaders :: map()}`

*Example*: `{status, 200, #{"content-type" => "application/json"}}`

Same as the simple interface but with an additional field for specifying additional headers.

#### Advanced interface

*Keyword*: `status`

*Spec*: `{status, Status :: integer(), ExtraHeaders :: map(), Body :: binary()}`

*Example*: `{status, 200, #{"content-type" => "text/plain"}, "A plain text"}`

Same as the medium interface but with an additional field for specifying a body.

### File transfers (Using cowboys sendfile functionality)

*Keyword*: `sendfile`

*Spec*: `{sendfile, StatusCode :: integer(), Headers :: map(), {Offset :: integer(), Length :: integer(), Path :: list()}, Mime :: binary()}`

*Example*: `{sendfile, 200, #{}, {0, 12345, "path/to/logo.png"}, "image/png"}`

Sends a file using sendfile. This uses cowboys sendfile functionality and more information about it can be found in the [cowboy manual on sendfile](https://ninenines.eu/docs/en/cowboy/2.9/guide/resp/#_sending_files)

### Redirecting user

*Keyword*: `redirect`

*Spec*: `{redirect, Route :: list() | binary()}`

*Example*: `{redirect, "/my/other/path}`

Sends a temporary redirect (HTTP status code 302) for the specified path to requester.


## Fallback controllers

[Phoenix](https://www.phoenixframework.org) have a really useful feature which they call [action_fallback]( https://hexdocs.pm/phoenix/Phoenix.Controller.html#action_fallback/1). We thought it would be a good addition to Nova to include somthing similar and therefore the `fallback_controller` was introduced. If a controller returns an invalid/unhandled result the fallback controller gets invoked and can take action on the payload. It's good for separating error-handling from the controllers. A fallback controller is set by setting the `fallback_controller` attribute with the module name of the fallback controller.

The following example shows how a controller defines a fallback

```
-module(my_main_controller).
-export([
    error_example/1
]).
-fallback_controller(my_fallback_controller).

error_example(_Req) ->
    %% Since {error, ...} is not a valid handler the fallback-controller will be invoked
    {error, example_error}.
```

A fallback controller have one function exposed; `resolve/2` which returnes a handler like for regular controllers in order to return the response to client. If we take the previous example and try and build a fallback controller for it:

```
-module(my_fallback_controller).
-export([
    resolve/2
]).

resolve(Req, {error, example_error}) ->
  {status, 400}.
```




## Plugins

Plugins is part of the Nova core and can be executed both before and after the execution of a controller. They can both terminate a request early (Like the *request plugin* does) or transform data into another structure (*json plugin*). Plugins can be defined in two different places; *Global* plugins is defined in the *sys.config* file and will be executed for every incoming request. If a plugin only should be executed for a limited set of endpoints it can be defined in the router-file for that specific application (These we call *local plugins*).

### Global plugins

Global plugins are defined in the *sys.config*-file and can have two different states; *pre*- and *post-controller* and is exectued before or after a controller. Global plugins lives under the `nova` application, `plugins`-key. An exampel of a sys.config file:

```
{nova, [
  {plugins, [
    {pre_request, [
      {nova_request_plugin, #{decode_json_body => true}}
    ]},
    {post_request, [
    ]}
  ]}
]}.
```

In order to find the valid options one can call the `plugin_info/0` function of each plugin. Currently there's [four plugins](https://github.com/novaframework/nova/tree/master/src/plugins) shipped with Nova today.


**NOTE!** If you are planning of creating an application that can be included in another Nova-application all of your plugins should be defined in the router-file (Local plugins) in order to avoid being overwritten.


### Local plugins

Local plugins works almost as the global ditos but for a limited set of paths. Local plugins is therefore declared in the router-file for each *group* of endpoints. They are declared in the same way as the global ones.

Example:
```
routes(_Env) ->
  [#{
    prefix => "/api/json",
    security => false,
    plugins => [
      {pre_request, [
        {nova_request_plugin, #{decode_json_body => true}}
      ]}
    ],
    routes => [
      {"/my_json_route", {my_app_json_controller, json}, #{methods => [get, post]}}
    ]
  }].
```

It's recommended to use this method of specifying routes if you plan to use it as a component in another *Nova application*.

## Websockets

*Coming soon*

### Callbacks

*Coming soon*
