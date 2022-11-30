# Watchers

Watchers is a concept that we directly ported from [Phoenix](https://www.phoenixframework.org/) and it enables the user to have running processes alongside the server.
It read the configuration from the `nova` application under `watchers`-key.

```
{nova, [
  {watchers, [
    {"os", "cmd", "node node_modules/bin/webpack.js --mode development --watch --watch-options-stdin", #{workdir => "priv/assets"}}}
   ]}
]}
```

The above example show how webpacks watch-command is started togheter with Nova. It will be run as a separate long-lived process and report back to the user (If dev-mode enabled in Nova)
