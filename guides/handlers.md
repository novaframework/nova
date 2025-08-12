# Handlers

Handlers in Nova are functions that process and transform the output returned by controllers before sending a response to the client. They are essential for formatting responses, setting headers, encoding data, and customizing how your application communicates with users or other systems.

## What are Handlers?

When a controller returns a result such as `{json, #{hello => world}}`, Nova uses handlers to convert that tuple into a properly formatted HTTP response. This includes setting the correct headers, encoding the payload (e.g., to JSON), and sending it out to the client.

Handlers are typically short, focused modules that define how to process specific types of controller return values.

## Creating Custom Handlers

You can create your own handlers to support custom response types or behaviors. Handlers are registered with Nova and invoked when a controller returns a tuple that matches their type.

**Example: Custom Console Handler**

```erlang
-module(my_console_handler).
-export([handle_console/4]).

handle_console({_Type, Format, Args}, _Callback, Req) ->
    io:format(Format, Args),
    {ok, Req}.
```

To use this handler, register it in your application's supervisor or initialization logic (See more in the 'Registering a handler'-section).

## Handler Return Types

A handler can return two different types:

- `{ok, Req}` – To continue the request process
- `{error, Reason}` – Triggers Nova to render a 500 error page to the user.

Remember that Nova is built around *Cowboy* so if you need to send a response to the requester then use `cowboy_req:reply/2`. Read more about cowboy_req at [https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_req](https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_req/)

## Built-in Handlers

Nova provides several built-in handlers for common response types, such as:

- **JSON Handler:** Automatically encodes maps or lists as JSON and sets the `content-type` header. Use like `{json, StatusCode, Headers, Map}`
- **HTML Handler:** Renders HTML templates and sets the appropriate headers. Use like `{ok, VariablesMap, OptionsMap}`
- **HTTP Status handler:** Returns a plain HTTP status without any content. Use like `{status, StatusCode}`

There's a few more and the easiest way to read more about them check the [docs](https://hexdocs.pm/nova/nova_basic_handler.html)

## Usage Example

Suppose your controller returns `{json, #{message => "Hello world"}}`. Nova's JSON handler will:

1. Encode the map to JSON.
2. Set the `content-type` header to `application/json`.
3. Return the HTTP response to the client.

**Controller Example:**

```erlang
-module(hello_controller).
-export([greet/1]).

greet(_Req) ->
    {json, #{message => "Hello world"}}.
```

## Registering a Handler

Handlers are registered using Nova’s API:

```erlang
nova_handlers:register_handler(custom_handler, my_custom_handler).
```

Once registered, when a controller returns `{custom_handler, Payload}`, Nova will use the specified handler to process the response.

## Advanced Usage

- You can define handlers for custom types, enabling new response patterns.
- Handlers can also perform post-processing, such as logging, analytics, or header manipulation.
- You may unregister or override default handlers if needed.

## Best Practices

- Keep handler logic focused and single-purpose.
- Use handlers to centralize output formatting and minimize duplication in controllers.
- Test custom handlers to ensure they react correctly to all expected and unexpected controller outputs.

---

For more details on available handlers or to see example implementations, check out the Nova source code or extend the documentation with your own custom handler recipe!s
