# Watchers

Watchers is a concept that we directly ported from [Phoenix](https://www.phoenixframework.org/) and it enables the user to have running processes alongside the server.
It read the configuration from the `nova` application under `watchers`-key.

```
{nova, [
  {watchers, [
    {my_application, "npm", ["run", "watch"], #{workdir => "priv/my_js_application"}}
   ]}
]}
```

The above example show how we invoke the `npm run watch` command in the `priv/my_js_application` directory. This will be a long-lived process and output will be forwarded to the erlang-console. Output from watchers will not be logged to log-files since it uses the `io:format/2` command to print messages.
