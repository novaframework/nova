# 04 - Controllers

## HTML

Controllers is located in `/src/controllers/` of your Nova application. A controller is basically a regular Erlang module but that exposes functions you've provided in
the routing file. Such a module can look something like this:

```erlang
-module(my_first_controller).
-export([
    index/1
    ]).

index(_NovaReq = #{req := #{method := <<"GET">>}}) ->
    {ok, []}.
```

`NovaReq` is a `map` that contains information from Nova. You can find the raw `cowboy_req` object there under the key `req`. So in the above example we are actually checking the method from the `cowbot_req` object given.
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

*Note; A controller does not need to have a corresponding view.*

## REST

A REST-controller is just a html-controller without the view part. You can return the same terms as for HTML-controller except for the `{ok, ...}` answer.

## Websockets

More info coming soon...
