# Quick start

## Start your first project

Nova is a *lightweight* and *modern* web framework for the BEAM VM, with full support for **Erlang**, **Elixir**, and **LFE**. It’s designed to help you build scalable, maintainable web applications with minimal setup. Whether you're new to BEAM languages or an experienced developer, Nova gives you a clean foundation to start building right away.


### Erlang approach

> #### Important
>
> If you require help installing Erlang and/or rebar3 please check [https://adoptingerlang.org/docs/development/setup/](https://adoptingerlang.org/docs/development/setup/)

#### Automated installation

*Via Curl*

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

*Via wget*
```bash
sh -c "$(wget -O- https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

#### Manual installation

Open your `rebar.config` file that should reside in `~/.config/rebar3/rebar.config*`. If the file does not exist you
can just create it. Locate the `plugins` section of the file and include the nova plugin. The result should look something like
the following:

```
{plugins,[{rebar3_nova,{git,"https://github.com/novaframework/rebar3_nova.git",
                            {branch,"master"}}}]}.
```


#### Creating the skeleton

After the installation of the Nova-plugin is done use rebar3 to generate a new project.

```bash
rebar3 new nova my_first_nova
```

```
$ rebar3 new nova my_first_app
===> Writing my_first_nova/config/dev_sys.config.src
===> Writing my_first_nova/config/prod_sys.config.src
===> Writing my_first_nova/src/my_first_nova.app.src
===> Writing my_first_nova/src/my_first_nova_app.erl
===> Writing my_first_nova/src/my_first_nova_sup.erl
===> Writing my_first_nova/src/my_first_nova_router.erl
===> Writing my_first_nova/src/controllers/my_first_nova_main_controller.erl
===> Writing my_first_nova/rebar.config
===> Writing my_first_nova/config/vm.args.src
===> Writing my_first_nova/src/views/my_first_nova_main.dtl
```

Now the skeleton have been created and you should be able to start it. Go into the newly created directory and run the `serve` command.

> #### Important!
>
> For the auto-compile/reload to work you need [inotify](https://github.com/massemanet/inotify) to be installed.

```
$ cd my_first_app
$ rebar3 nova serve
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


### Elixir approach

#### Install mix task

In order to install Novas mix task, just run;

```elixir
mix archive.install hex nova_new
```

Once installed you will have the option to create a new Nova project using

```elixir
mix nova.new my_app
```

Now you can run

```bash
cd my_app
mix deps.get
iex -S mix
```

If you take a look at [http://localhost:8080](http://localhost:8080) you should be greeted by a Nova-page.
