# Views

Nova renders views with [ErlyDTL](https://github.com/erlydtl/erlydtl), an
Erlang implementation of the Django template language. Templates are compiled
to Erlang modules at build time.

## Build setup

A project created with `rebar3 new nova` has this in place. In `rebar.config`
the `rebar3_erlydtl_plugin` compiles the templates before the Erlang sources:

```erlang
{erlydtl_opts, [
    {doc_root, "src/views"},
    {recursive, true},
    {libraries, [
        {nova_erlydtl_inventory, nova_erlydtl_inventory}
    ]},
    {default_libraries, [nova_erlydtl_inventory]}
]}.

{provider_hooks, [
    {pre, [{compile, {erlydtl, compile}}]}
]}.
```

A template `src/views/profile.dtl` is compiled to the module `profile_dtl`.

## Creating a view

Put the template in `src/views`:

```html+django
<h1>Welcome, {{ name }}!</h1>
```

## Rendering a view from a controller

`{ok, Variables}` and `{view, Variables}` render a view. The two atoms mean
the same.

```erlang
-module(my_app_main_controller).
-export([index/1, profile/1]).

index(_Req) ->
    {ok, #{name => ~"Alice"}}.

profile(_Req) ->
    {ok, #{name => ~"Alice"}, #{view => profile}}.
```

Without a `view` option the view is named after the controller: the module
name with a trailing `_controller` removed. `index/1` above renders
`src/views/my_app_main.dtl`. `profile/1` renders `src/views/profile.dtl`.

`Variables` is a map or a property list. Its keys are the variables of the
template.

The options map takes:

| Key | Description |
|---|---|
| `view` | The template to render, as an atom or a string, without the `.dtl` extension |
| `status_code` | The status code of the response, `200` by default |
| `headers` | Headers of the response. They are added to the default `content-type: text/html`, which stays unless they name a `content-type` |

When the template does not exist the response is the `500` page, and Nova
logs `Nova could not render template` with the name of the template. In
`dev` the page names the template as well.

## Escaping

ErlyDTL escapes the value of a variable for HTML by default.
`{{ comment }}` with the value `<script>alert(1)</script>` renders as
`&lt;script&gt;alert(1)&lt;/script&gt;`. The `safe` filter turns that off for
one value:

```html+django
<p>{{ comment }}</p>
<p>{{ trusted_html|safe }}</p>
```

Use `safe` only on values your application produced. The `auto_escape`
option in `erlydtl_opts` turns escaping off for every template; leave it on.

## The csrf_token variable

When `nova_csrf_plugin` runs for the route, Nova adds the token to the
variables as `csrf_token`:

```html+django
<form method="post">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
  <button>Save</button>
</form>
```

See [Plugins](plugins.md) for the plugin.

## The url tag

`nova_erlydtl_inventory` adds a `url` tag that puts the prefix an application
is mounted under in front of a path:

```html+django
<a href="{% url "/items" application="my_app" %}">Items</a>
```

For an application included with `{admin_app, #{prefix => "/admin"}}` the tag
renders `/admin/items`. For an application Nova has not compiled, and for a
tag without `application`, it renders `#` and logs a warning.

## Template features

ErlyDTL supports template inheritance (`{% extends %}`), includes
(`{% include %}`), control structures (`{% if %}`, `{% for %}`), filters and
custom tags. See the [ErlyDTL wiki](https://github.com/erlydtl/erlydtl/wiki)
for the syntax.
