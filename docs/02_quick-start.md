# 02 - Quick start

## Use rebar3 and nova_rebar3

The easiest way to get started with nova is to make use of the rebar3 plugin which can be found here: https://github.com/burbas/nova_rebar3
Once installed you can call

```
$ rebar3 new nova my_first_app
===> Writing my_first_app/priv/my_first_app.routes.erl
===> Writing my_first_app/src/my_first_app.app.src
===> Writing my_first_app/src/my_first_app_app.erl
===> Writing my_first_app/src/my_first_app_sup.erl
===> Writing my_first_app/src/controllers/my_first_app_main_controller.erl
===> Writing my_first_app/src/views/my_first_app_main_controller.dtl
===> Writing my_first_app/rebar.config
```

This will create a bunch of boiler plate files that helps you getting started. To start your app just type

```
$ rebar3 shell
===> Verifying dependencies...
===> Compiling my_first_app
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1>
```

Now you can type `application:ensure_all_started(my_first_app).` to start your app. If you now open http://localhost:8080/ you should see a "Hello World"-message.

## Dev mode

When developing you probably don't want to quit erlang, recompile and then re-run the whole thing again. If you specify `{devmode, true}` in your configuration the `nova_reloader` module will start.
This module checks for file changes and recompiles/reloades the files if needed. You can also add additional directories with `nova_reloader:add_directory/1`.

*Note!* We semi-support reloading dtl-templates at this stage, but we'll fix this asap.
