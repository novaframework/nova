# Watchers

A watcher is an external command that Nova starts together with your application, such as an asset compiler that rebuilds on file changes. The concept comes from the [Phoenix Framework](https://www.phoenixframework.org/).

## What are Watchers?

A watcher is an OS process started when Nova starts, for instance `npm run watch` for JavaScript assets. Watchers are mostly useful during development. They are started in every environment, so configure them in the development configuration only.

## Configuration

Watchers are configured in your `sys.config` file under the `watchers` key of your *bootstrap application*, the one named by `bootstrap_application`. They are not read from the `nova` application. Each entry is a tuple in one of these forms:

| Entry | Meaning |
|-------|---------|
| `{Command}` | Run `Command` in the directory of the bootstrap application |
| `{Command, Arguments}` | As above, with a list of arguments |
| `{Command, Options}` | As above, with an options map |
| `{Application, Command, Options}` | Run `Command` in the directory of `Application` |
| `{Application, Command, Arguments, Options}` | As above, with a list of arguments |

`Command` is a string and `Arguments` a list of strings. The only option is `workdir`, a directory relative to the application's directory (an absolute path is used as it is).

**Example: Running a JavaScript Asset Watcher**

```erlang
{my_application, [
  {watchers, [
    {my_application, "npm", ["run", "watch"], #{workdir => "priv/my_js_application"}}
  ]}
]}
```

Nova starts `npm run watch` inside the `priv/my_js_application` directory of `my_application` when it starts.

## How Watchers Work

- Watchers are started as OS processes by the Nova supervision tree, in every environment. Leave the `watchers` key out of the configuration of a release that should not run them.
- The command line is run through the system shell.
- The command runs in its own working directory. The working directory of the Erlang node is not changed.
- Output from watcher processes is printed to the Erlang console when the `environment` is `dev`, and dropped otherwise. It is not written to Nova log files.
- A watcher that exits is not restarted. A warning is logged when it exits abnormally.
- A watcher that cannot be started (an unknown application, or arguments that are not strings, for example) is logged as the error `Could not start watcher` and skipped. The other watchers are started.
