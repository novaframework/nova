# Rebar3 Nova

Nova comes with a set of handy [rebar3](https://www.rebar3.org/) commands to make development and management of your Nova projects easier. These commands are provided by the `rebar3_nova` plugin, which is typically installed automatically when you follow Nova’s installation instructions.

## Available Commands

### `rebar3 new nova <project-name>`

Creates a new Nova project scaffold in the directory `<project-name>`. This sets up the recommended directory structure, configuration files, and example modules so you can get started quickly.

### `rebar3 nova serve`

Starts a local web server using the Nova framework. The server will listen on the port specified in your `sys.config` file. This command also enables hot code reloading, so any changes to your code are automatically reflected without restarting the server.

### `rebar3 nova routes`

Prints a list of all routes configured in your project, including those from any included Nova applications. This is helpful for quickly reviewing your application’s endpoints.

---

## Example Workflow

1. **Create a new project:**
    ```sh
    $ rebar3 new nova my_app
    $ cd my_app
    ```

2. **Start the development server:**
    ```sh
    $ rebar3 nova serve
    ```

3. **Check your routes:**
    ```sh
    $ rebar3 nova routes
    ```

---

## Future Features

The Nova team plans to expand the set of available rebar3 commands. If you have suggestions or feature requests, please open an issue on the [Nova GitHub repository](https://github.com/novaframework/nova/issues).

---

For more details on using rebar3, see the [official rebar3 documentation](https://www.rebar3.org/docs/getting-started).
