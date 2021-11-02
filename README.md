![nova logo](https://raw.githubusercontent.com/novaframework/nova/master/priv/static/nova.png)

> ### Simple. Fault-tolerant. Distributed.
> - Create a basic webpage in minutes
> - Using Erlang OTP to achieve both fault-tolerance and distribution

[http://www.novaframework.org](http://www.novaframework.org)

[![Build Status](https://travis-ci.org/novaframework/nova.svg?branch=master)](https://travis-ci.org/novaframework/nova)

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

Nova is supported with OTP 22 and above.

## Documentation

Hex docs: https://hexdocs.pm/nova/

More on how things work can be read in the docs [Introduction](docs/).

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
