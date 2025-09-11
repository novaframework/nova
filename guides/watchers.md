# Watchers

Watchers in Nova allow you to run background processes or commands alongside your web application. This concept is inspired by the [Phoenix Framework](https://www.phoenixframework.org/), providing a way to automate tasks such as asset compilation, monitoring, or other long-running jobs.

## What are Watchers?

A watcher is a process started when your Nova application launches. You can use watchers to run external commands (like `npm run watch` for JavaScript assets) or any other tool that needs to operate continuously while your application is running.

Watchers are especially useful for development workflows, but can also be used in production for tasks like real-time log monitoring, notification systems, or custom daemons.

## Configuration

Watchers are configured in your application's `sys.config` file under the `nova` application's `watchers` key. Each watcher entry specifies:

- The application name (for grouping)
- The command to run
- The command arguments
- Optional settings (such as working directory)

**Example: Running a JavaScript Asset Watcher**

```erlang
{nova, [
  {watchers, [
    {my_application, "npm", ["run", "watch"], #{workdir => "priv/my_js_application"}}
  ]}
]}
```

In this example, Nova will start `npm run watch` inside the `priv/my_js_application` directory when the server launches. This keeps your JavaScript assets up to date automatically.

## How Watchers Work

- Watchers are started as OS processes by the Nova supervision tree.
- Output from watcher processes is forwarded to the Erlang console using `io:format/2`. Note: Output is not written to Nova log files by default.
- Watchers are restarted if they crash, depending on your supervision strategy.

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
