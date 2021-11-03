# Quick start

## Use rebar3 and nova

Start by add the rebar3 template for Nova.

*Via Curl*

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/nova/master/tools/install.sh)"
```

*Via wget*
```bash
sh -c "$(wget -O- https://raw.githubusercontent.com/novaframework/nova/master/tools/install.sh)"
```

After this is done use rebar3 to generate a new project with Nova.

```bash
rebar3 new nova my_first_nova
```

```
$ rebar3 new nova my_first_app
===> Writing my_first_app/config/sys.config
===> Writing my_first_app/start.sh
===> Writing my_first_app/priv/my_first_app.routes.erl
===> Writing my_first_app/src/my_first_app.app.src
===> Writing my_first_app/src/my_first_app_app.erl
===> Writing my_first_app/src/my_first_app_sup.erl
===> Writing my_first_app/src/controllers/my_first_app_main_controller.erl
===> Writing my_first_app/rebar.config
===> Writing my_first_app/src/views/my_first_app_main.dtl
```

This will create a bunch of boiler plate files that helps you getting started. To start your app just type:

```
$ cd my_first_app
$ rebar3 shell
===> Verifying dependencies...
===> Compiling my_first_app
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]
...
a lot of progress-reports
...
===> Booted my_first_app
...
```

If you take a look at [http://localhost:8080](http://localhost:8080) you should be greeted by a Nova-page.


## Nova admin

This is a project we hope to be presenting with the nova release 0.5.
