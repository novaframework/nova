# nova_roadrunner

Run [Nova](https://github.com/novaframework/nova) on
[Roadrunner](https://github.com/arizona-framework/roadrunner).

```erlang
{deps, [nova, nova_roadrunner]}.
```

Add `nova_roadrunner` to the `applications` of your application, after
`nova`:

```erlang
{applications, [kernel, stdlib, nova, nova_roadrunner]}
```

Serves HTTP/1.1, HTTP/2 and HTTP/3, and WebSockets over HTTP/1.1:

```erlang
{nova, [
    {listeners, #{
        web => #{port => 8443,
                 tls => #{certfile => "cert.pem", keyfile => "key.pem"},
                 protocols => [http1, http2, http3]}
    }}
]}
```

See the `nova_roadrunner` module for the options a listener takes and for
what differs from Cowboy, and `nova_listener` for how listeners are
configured.

Derived from [roadrunner_nova](https://github.com/arizona-framework/roadrunner_nova)
by William Fank Thomé.
