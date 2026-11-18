# nova_cowboy

Run [Nova](https://github.com/novaframework/nova) on
[Cowboy](https://github.com/ninenines/cowboy).

```erlang
{deps, [nova, nova_cowboy]}.
```

Add `nova_cowboy` to the `applications` of your application, after `nova`:

```erlang
{applications, [kernel, stdlib, nova, nova_cowboy]}
```

Serves HTTP/1.1 and HTTP/2, with or without TLS, and WebSockets. See the
`nova_cowboy` module for the options a listener takes, and `nova_listener`
for how listeners are configured.
