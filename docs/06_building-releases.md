# 06 Building releases



## Include other Nova applications

You can include other nova applications as part of your release. Just include them as regular rebar3-deps and
bootstrap them by telling your application to include them.

If you would include the application `nova_admin` into your app `testapp`, your sys.config could look something like this:

```erlang
{nova_applications, [
    #{name => "testapp",
      routes_file => "priv/testapp.routes.erl"},
    #{name => "nova_admin",
      prefix => "/admin"}
    ]}.
```

This tells nova that you want to include the `nova_admin` application and prefix all of its paths with `/admin`.
