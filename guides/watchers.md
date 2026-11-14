# Watchers

Watchers in Nova allow you to run background processes or commands alongside your web application. This concept is inspired by the [Phoenix Framework](https://www.phoenixframework.org/), providing a way to automate tasks such as asset compilation, monitoring, or other long-running jobs.

## What are Watchers?

A watcher is a process started when your Nova application launches. You can use watchers to run external commands (like `npm run watch` for JavaScript assets) or any other tool that needs to operate continuously while your application is running.

Watchers are especially useful for development workflows, but can also be used in production for tasks like real-time log monitoring, notification systems, or custom daemons.

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

In this example, Nova will start `npm run watch` inside the `priv/my_js_application` directory of `my_application` when the server launches. This keeps your JavaScript assets up to date automatically.

## How Watchers Work

- Watchers are started as OS processes by the Nova supervision tree, in every environment. Leave the `watchers` key out of the configuration of a release that should not run them.
- The command line is run through the system shell.
- The command runs in its own working directory. The working directory of the Erlang node is not changed.
- Output from watcher processes is printed to the Erlang console when the `environment` is `dev`, and dropped otherwise. It is not written to Nova log files.
- A watcher that exits is not restarted. A warning is logged when it exits abnormally.
- A watcher that cannot be started (unknown application, for example) is logged as an error and skipped.

## Best Practices

- Use watchers for tasks that need to run for the lifetime of your application.
- Keep watcher commands simple and reliable; if a watcher process fails, it can affect your development or deployment workflow.
- Organize watcher-related files (like build scripts) in your project’s `priv` or other dedicated directories.

## Advanced Use Cases

- Run multiple watchers for different applications or tasks.
- Run file watchers, asset compilers, or custom Erlang processes.
- Use watchers in production for monitoring, alerts, or data collection.

---

Watchers are a powerful tool for automating your workflow in Nova. For more advanced usage, check the Nova source code or community resources!
