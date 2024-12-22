# Cookies / Sessions

Cookies in Nova are handled by the `nova_session` module. This module is responsible for setting and getting cookies, as well as managing the session data. This means that you can store data in cookies without having to worry about session ids or anything like that - it's all handled for you.

## API

### `nova_session:set_value/3`

```erlang
-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: binary()) ->
                 ok | {error, Reason :: atom()} | no_return().
```

This function is used to set a cookie. It takes two arguments: the request object, the name of the cookie and the value of the cookie.

```erlang
nova_session:set_value(Req, "name", "John Doe").
```

### `nova_session:get_value/2`

```erlang
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
          {ok, Value :: binary()} | {error, Reason :: atom()} | no_return().
```

This function is used to get a cookie. It takes two arguments: the request object and the name of the cookie.

```erlang
nova_session:get_value(Req, "name").
```

### `nova_session:delete/2`

```erlang
-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} |
                                                          {error, Reason :: atom()} | no_return().
```

This function is used to delete a value. It takes two arguments: the request object and the name of the value.

```erlang
nova_session:delete(Req, "name").
```

### `nova_session:delete/1`

```erlang
-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} |
                                           {error, Reason :: atom()} | no_return().
```

This function is used to delete all values. It takes one argument: the request object.

```erlang
nova_session:delete(Req).
```

## Enabling Cookies

Cookies are not enabled by default in Nova, but if you need to enable them, you can do so by setting the `use_sessions` configuration option to `true`. This will enable cookies for the entire site.
It's also possible to set the backend for the session data. By default, the session data is stored in an *ETS* table in the Erlang VM but it's quite easy to implement a custom backend.

## Backend

You can implement your own backend by creating a module that implements the `nova_session` behaviour. This behaviour has four functions that need to be implemented: `get_value/2`, `set_value/3`, `delete_value/1` and `delete_value/2`.

```erlang
-module(my_session_backend).
-behaviour(nova_session).

-export([get_value/2, set_value/3, delete_value/1, delete_value/2]).

get_value(Req, _Key) ->
    %% Your code here,
    {ok, Req}.

set_value(Req, _Key, _Value) ->
    %% Your code here,
    {ok, Req}.

delete_value(Req) ->
    %% Your code here,
    {ok, Req}.

delete_value(Req, _Key) ->
    %% Your code here,
    {ok, Req}.
```

This module can then be set as the backend in the `sys.config` file under `nova`.

```erlang
{session_manager, my_session_backend}.
```
