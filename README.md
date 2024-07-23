![nova logo](https://raw.githubusercontent.com/novaframework/nova/master/priv/static/nova.png)

> ### Simple. Fault-tolerant. Distributed.
> - Create a basic webpage in minutes
> - Using Erlang OTP to achieve both fault-tolerance and distribution

[http://www.novaframework.org](http://www.novaframework.org)

![Build status](https://github.com/novaframework/nova/actions/workflows/erlang.yml/badge.svg)


## Getting started

Start by adding the `rebar3` template for Nova. This can be done by running the installation script;

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

#### Manually with rebar.config

Add rebar3_nova to ~/.config/rebar3/rebar.config
```erlang
{project_plugins, [rebar3_nova]}
```

After this is done use `rebar3` to generate a new project with Nova.

```bash
rebar3 new nova my_first_nova
```




## Supported Erlang versions

Nova is supported with OTP 23 and above.

## Documentation

Hex docs: https://hexdocs.pm/nova/

More on how things work can be read in the docs [Getting Started](https://hexdocs.pm/nova/quick-start.html#content).

### Articles

* [Building an Erlang Web Api using Nova Framework and Redis](https://bercovici-adrian-simon.medium.com/building-an-erlang-web-api-using-nova-framework-and-redis-141edf170ef7)
* [Gettings started with Nova](https://dev.to/taure/getting-started-with-nova-1ioo)

## Contributing

Contribution is welcome to Nova. Check our [CODE OF CONDUCT](CODE_OF_CONDUCT.md) for more information. We will add features and bugs to the issues list.

### Generating a Nova project

Start a new project with:

```bash
rebar3 new nova my_first_nova
```

That will generate a Nova project for you.

```bash
rebar3 nova serve
```

This will fetch all dependencies and compile. After the compilation it will start a shell that says which port it is running on and a few debug lines.

When the shell is started, open a browser and go to localhost:8080 which will point to the `my_first_nova` server running Nova.

## Important links

* [Erlang Slack channel][1]
* [Issue tracker][2]
* [Nova Forum (questions)][3]
* Visit Nova's sponsors for expert Nova consulting:
    * [Burbas Consulting](http://burbasconsulting.com)
    * [Widgrens IT](https://widgrensit.com)

[1]: https://erlef.org/slack-invite/erlanger
[2]: https://github.com/novaframework/nova/issues
[3]: https://erlangforums.com/c/erlang-frameworks/nova-forum/65
