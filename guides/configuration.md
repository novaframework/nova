# Configuration parameters

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value |
|-----|-------------|-------|
| `use_persistent_term` | Use `persistent_term` module to store routing tree | `boolean()` |
| `render_error_pages` | If Nova should render error-pages for HTML-request | `boolean()` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` |
| `session_manager` | Specifify a module to use as the session manager. Defaults to `nova_session_ets` | `atom()` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` |
| `cowboy_configuration` | If you need some additional configuration done to Cowboy this is the place. Check `nova_sup` module to learn which keys that can be defined. | `map()` |

# Application parameters

These parameters can be specified in your *main* application (Eg the one you've specified in the `bootstrap`-section).

| Key | Description | Value |
|-----|-------------|-------|
| `json_lib` | JSON lib to use. Read more in the subsection *Configure json lib* | `atom()` |
| `watchers` | Watchers are external programs that will run together with Nova. Watchers are defined as list of tuples where the tuples is in format `{Command, ArgumentList}` (Like `[{my_app, "npm", ["run", "watch"], #{workdir => "priv/assets/js/my-app"}}]`) | `[{string(), string()}] | [{atom(), string(), map()}] | [{atom(), string(), list(), map()}]` |



## Configure json_lib

One can configure which json library to use for encoding/decoding json structures. The module defined for this should expose two different functions:

`encode(Structure) -> binary() | iolist()`

`decode(JsonString) -> {ok, Structure}`
