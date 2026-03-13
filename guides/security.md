# Security

This guide covers security best practices for Nova applications, based on the [ERLEF Web App Security Best Practices](https://security.erlef.org/web_app_security_best_practices_beam/) and [Phoenix security recommendations](https://hexdocs.pm/phoenix/security.html).

## Session Security

Nova stores sessions server-side in ETS by default (not in cookies), which is already more secure than client-side cookie storage. Session cookies are set with security attributes:

- **HttpOnly** — prevents JavaScript from reading the session cookie (XSS mitigation)
- **Secure** — cookie only sent over HTTPS
- **SameSite=Lax** — additional CSRF protection
- **Path=/** — cookie scoped to entire site

These can be customized in your *sys.config*:

```erlang
{nova, [
    {session_cookie_opts, #{secure => false}}  %% For local development over HTTP
]}
```

### Session Timeouts

Sessions have configurable maximum age and idle timeout:

```erlang
{nova, [
    {session_max_age, 86400},      %% Absolute max age in seconds (default: 24 hours)
    {session_idle_timeout, 3600}   %% Idle timeout in seconds (default: 1 hour)
]}
```

Expired sessions are automatically cleaned up periodically.

### Session Rotation

To prevent session fixation attacks, rotate the session ID when a user's privilege level changes (e.g. after login):

```erlang
login(Req) ->
    %% ... validate credentials ...
    {ok, Req1} = nova_session:rotate(Req),
    nova_session:set(Req1, <<"user_id">>, UserId),
    {ok, #{}, Req1}.
```

This generates a new session ID, migrates session data, and sets a new cookie.

## CSRF Protection

Nova includes the `nova_csrf_plugin` which uses the synchronizer token pattern. Enable it in your plugin configuration:

```erlang
{pre_request, nova_csrf_plugin, #{}}
```

The plugin:
- Generates a cryptographically random token per session
- Validates tokens on state-changing requests (POST, PUT, PATCH, DELETE)
- Skips safe methods (GET, HEAD, OPTIONS)
- Uses constant-time comparison to prevent timing attacks

Include the CSRF token in your forms:

```html
<form method="post" action="/submit">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />
    ...
</form>
```

Or send it via header for AJAX requests:

```javascript
fetch('/api/data', {
    method: 'POST',
    headers: {'X-CSRF-Token': csrfToken}
});
```

### Options

```erlang
{pre_request, nova_csrf_plugin, #{
    field_name => <<"_csrf_token">>,         %% Form field name
    header_name => <<"x-csrf-token">>,       %% Header name
    session_key => <<"_csrf_token">>,         %% Session storage key
    excluded_paths => [<<"/api/webhooks">>]   %% Paths to skip
}}
```

**Important:** Never allow state-changing operations via GET requests.

## HTTP Security Headers

The `nova_secure_headers_plugin` sets security headers on every response:

```erlang
{pre_request, nova_secure_headers_plugin, #{}}
```

Default headers:

| Header | Default Value | Purpose |
|--------|--------------|---------|
| `X-Frame-Options` | `DENY` | Prevents clickjacking |
| `X-Content-Type-Options` | `nosniff` | Prevents MIME sniffing |
| `X-XSS-Protection` | `1; mode=block` | Legacy XSS protection |
| `Referrer-Policy` | `strict-origin-when-cross-origin` | Controls referrer information |
| `Permissions-Policy` | `geolocation=(), camera=(), microphone=()` | Restricts browser features |

### HSTS (HTTP Strict Transport Security)

Enable HSTS to prevent downgrade attacks:

```erlang
{pre_request, nova_secure_headers_plugin, #{
    hsts => true,
    hsts_max_age => 31536000,           %% 1 year (default)
    hsts_include_subdomains => true      %% default
}}
```

### Content Security Policy

Set a CSP to prevent XSS and data injection:

```erlang
{pre_request, nova_secure_headers_plugin, #{
    csp => <<"default-src 'self'; script-src 'self'; style-src 'self'">>
}}
```

### Overriding Defaults

```erlang
{pre_request, nova_secure_headers_plugin, #{
    headers => #{<<"x-frame-options">> => <<"SAMEORIGIN">>},   %% Override defaults
    extra_headers => #{<<"x-custom">> => <<"value">>}          %% Add new headers
}}
```

## TLS / HTTPS

### Force SSL

The `nova_force_ssl_plugin` redirects HTTP requests to HTTPS:

```erlang
{pre_request, nova_force_ssl_plugin, #{
    excluded_paths => [<<"/health">>]   %% Skip health check endpoints
}}
```

Use this together with HSTS for complete TLS enforcement.

### SSL Configuration

Configure SSL in your *sys.config*:

```erlang
{nova, [
    {cowboy_configuration, #{
        ssl => #{
            port => 8443,
            ssl_options => #{
                certfile => "/path/to/fullchain.pem",
                keyfile => "/path/to/privkey.pem"
            }
        }
    }}
]}
```

## Rate Limiting

The `nova_rate_limit_plugin` protects against brute-force and DoS attacks:

```erlang
{pre_request, nova_rate_limit_plugin, #{
    max_requests => 100,        %% Per window (default)
    window_ms => 60000,         %% 1 minute (default)
    paths => [<<"/api/login">>, <<"/api/register">>]   %% Limit specific paths
}}
```

When the limit is exceeded, the plugin returns `429 Too Many Requests` with a `Retry-After` header.

### Custom Rate Limit Keys

By default, requests are tracked by client IP. Use a custom key function for other strategies:

```erlang
{pre_request, nova_rate_limit_plugin, #{
    max_requests => 1000,
    window_ms => 3600000,
    key_fun => fun(#{headers := Headers}) ->
        maps:get(<<"x-api-key">>, Headers, <<"anonymous">>)
    end
}}
```

## CORS

The `nova_cors_plugin` sets Access-Control headers with restrictive defaults:

```erlang
{pre_request, nova_cors_plugin, #{
    allow_origins => <<"https://app.example.com">>
}}
```

Default values:
- `allow_headers`: `Content-Type, Authorization`
- `allow_methods`: `GET, POST, PUT, DELETE, OPTIONS`

**Avoid** using wildcard `*` for origins in production. Be explicit:

```erlang
{pre_request, nova_cors_plugin, #{
    allow_origins => <<"https://app.example.com">>,
    allow_headers => <<"Content-Type, Authorization, X-Custom-Header">>,
    allow_methods => <<"GET, POST">>
}}
```

## Cross-Site Scripting (XSS)

Nova uses erlydtl (Django Template Language) which **auto-escapes variables by default**:

```html
{{ user_input }}       <!-- Auto-escaped: <script> becomes &lt;script&gt; -->
{{ user_input|safe }}  <!-- DANGEROUS: bypasses escaping -->
```

**Never** use the `|safe` filter with user-supplied data.

### Additional XSS Vectors

- **`{html, Body}`** — If your controller returns raw HTML, ensure user input is escaped
- **JSON responses** — `json:encode/1` handles encoding safely
- **Content-Type headers** — Never let users control the `Content-Type` response header
- **File uploads** — Validate and restrict content types for uploaded files

## SQL Injection

If using Kura or parameterized queries, you're protected by default:

```erlang
%% Safe — parameterized query
kura:all(users, #{where => #{email => UserInput}}).

%% Safe — explicit parameters
Sql = "SELECT * FROM users WHERE email = $1",
pgo:query(Sql, [UserInput]).

%% DANGEROUS — string interpolation
Sql = "SELECT * FROM users WHERE email = '" ++ UserInput ++ "'".
```

**Never** interpolate user input into SQL strings.

## Authorization

Nova provides `nova_security_handler` for per-route authorization. Always enforce authorization server-side:

```erlang
%% In your router
{"/admin/users", {admin_controller, index}, #{secure => {auth_module, admin_check}}}
```

```erlang
%% auth_module.erl
admin_check(Req) ->
    case nova_session:get(Req, <<"role">>) of
        {ok, <<"admin">>} -> {true, #{role => admin}};
        _ -> {false, 403, [], <<"Forbidden">>}
    end.
```

Never rely on client-side hiding for access control. Always validate on the server.

## Logging

### Parameter Filtering

Use `nova_log_filter` to prevent sensitive data from appearing in logs:

```erlang
%% sys.config — configure which parameters to filter
{nova, [
    {filter_parameters, [<<"password">>, <<"secret">>, <<"token">>,
                         <<"api_key">>, <<"credit_card">>]}
]}
```

Add the filter to your logger configuration:

```erlang
{kernel, [{logger, [
    {handler, default, logger_std_h, #{
        filters => [{nova_param_filter,
            {fun nova_log_filter:filter/2, #{}}}]
    }}
]}]}
```

You can also use `nova_log_filter:redact_params/1` directly:

```erlang
FilteredParams = nova_log_filter:redact_params(Params).
%% #{<<"username">> => <<"alice">>, <<"password">> => <<"[FILTERED]">>}
```

### Error Pages in Production

Disable detailed error pages and stacktraces in production:

```erlang
{nova, [
    {render_error_pages, false},
    {use_stacktrace, false}
]}
```

## BEAM-Specific Concerns

### Atom Exhaustion

Atoms in the BEAM are never garbage collected. Never create atoms from user input:

```erlang
%% DANGEROUS — each unique input creates a permanent atom
binary_to_atom(UserInput).
list_to_atom(UserInput).

%% Safe — only succeeds for existing atoms
binary_to_existing_atom(UserInput).
list_to_existing_atom(UserInput).
```

### Binary Deserialization

Never deserialize untrusted binary data:

```erlang
%% DANGEROUS — can execute arbitrary code
binary_to_term(UserInput).

%% Slightly safer but still risky — prevents atom creation only
binary_to_term(UserInput, [safe]).
```

Use JSON or another safe format for data exchange with untrusted sources.

### Remote Code Execution

Never pass user input to:

- `os:cmd/1`
- `:erlang.apply/3` with user-controlled module/function
- `code:load_binary/3`

## Recommended Plugin Stack

A secure plugin configuration for production:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_force_ssl_plugin, #{
            excluded_paths => [<<"/health">>]
        }},
        {pre_request, nova_secure_headers_plugin, #{
            hsts => true,
            csp => <<"default-src 'self'">>
        }},
        {pre_request, nova_rate_limit_plugin, #{
            max_requests => 100,
            window_ms => 60000,
            paths => [<<"/api/login">>, <<"/api/register">>]
        }},
        {pre_request, nova_request_plugin, #{}},
        {pre_request, nova_csrf_plugin, #{
            excluded_paths => [<<"/api/webhooks">>]
        }},
        {pre_request, nova_cors_plugin, #{
            allow_origins => <<"https://app.example.com">>
        }}
    ]}
]}
```

## Further Reading

- [ERLEF Web Application Security Best Practices](https://security.erlef.org/web_app_security_best_practices_beam/)
- [ERLEF Secure Coding and Deployment Hardening Guidelines](https://security.erlef.org/)
- [Phoenix Security Guide](https://hexdocs.pm/phoenix/security.html)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Mozilla Server-Side TLS](https://wiki.mozilla.org/Security/Server_Side_TLS)
