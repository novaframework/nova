# 04 - Controllers

Controllers is located in `/src/controllers/` of your Nova application. A controller is basically a regular Erlang module but that exposes functions you've provided in
the routing file. Such a module can look something like this:

```erlang
-module(my_first_controller).
-export([
    index/1
    ]).

index(#{method := <<"GET">>}) ->
    {ok, []}.
```

This means that if you have a route pointing to it, this module will only take GET-requests and will return a rendered template to the user. Returning `{ok, Parameters}` from a
module means that it will render the corresponding template and return it to the user.

A controller can return a set of different values;

`{json, Map}` will return a JSON-object to the user

`{json, StatusCode, HeadersMap, Map}` Same as the above but will now also signal a different HTTP-code aswell as custom headers

`{ok, Variables}` Will render the template with `Variables`

`{ok, Variables, Options}` Same as the above but with additional options **TODO** Explain the options

`{status, StatusCode}` Sends out a plain status code. If there's a corresponding route for this status code the route will be called

`{status, StatusCode, ExtraHeaders}` Same as the above but with additional headers

`{redirect, Route}` Returns a 302 togheter with a route to the user

`{cowboy_req, Req}` This is a special return value that can be used if you want todo something extra that nova doesn't support but cowboy does.


*Note; A controller does not need to have a corresponding view.*
