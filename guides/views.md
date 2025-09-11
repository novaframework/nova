# Views

Nova uses the [erlydtl](https://github.com/erlydtl/erlydtl) template engine to power its view layer. erlydtl is an Erlang implementation of the Django template language, which makes templates both expressive and familiar to developers with experience in Django or similar frameworks.

## How Views Work in Nova

Views are responsible for rendering the final output sent to the client, typically HTML, but you can also use them for other formats. Nova compiles your templates to efficient Erlang code at build time, ensuring fast rendering.

### Creating a View

To create a view, place your template files (with the `.dtl` extension) in your application's `src/views` directory. For example:

```
src/views/
  └── user_profile.dtl
```

A sample template might look like this:

```html+django
<h1>Welcome, {{ name }}!</h1>
<p>Email: {{ email }}</p>
```

### Rendering a View from a Controller

In your controller, you can render a view using Nova’s built-in helpers:

```erlang
Vars = #{name => "Alice", email => "alice@example.com"},
{ok, Vars, #{view => user_profile}}.
```

The example above will then render the *user_profile.dtl* template.

### Passing Data to Templates

You pass variables to the template as a map. All keys in the map become available in the template as variables.

### Template Features

erlydtl supports:

- **Template inheritance** (`{% extends %}`)
- **Includes** (`{% include %}`)
- **Control structures** (`{% if %}`, `{% for %}`)
- **Filters** (`{{ value|escape }}`)
- **Custom tags and filters** (advanced usage)

For details on syntax and features, see the [erlydtl wiki](https://github.com/erlydtl/erlydtl/wiki).

### Error Handling

If a template is missing or a rendering error occurs, Nova will log the error and, depending on configuration, may return a generic error page or propagate the details in development mode.

### Best Practices

- Organize templates in subfolders for maintainability.
- Use template inheritance to avoid duplication.
- Validate your data before passing it to the template.

---

For more about advanced template syntax, custom filters, and extending views, read the [erlydtl documentation](https://github.com/erlydtl/erlydtl/wiki).
