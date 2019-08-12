![nova logo](https://raw.githubusercontent.com/novaframework/nova/master/priv/static/nova.png)

> ### Simple. Fault-tolerant. Distributed.
> A web framework.

## Getting started

Start by add the rebar3 template for Nova.

```bash
cd installer
./installer.sh
```

After this is done use rebar3 to generate a new project with Nova.

```bash
rebar3 new nova my_first_nova
```

We will add a homepage soon.

## Documentation

Hex docs: https://hexdocs.pm/nova/

More how things is working can be read in docs [Introduction](docs/01_introduction.md).

## Contributing

Contribution is welcome to Nova. Check our [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for more information. We will add features and bugs in the issues list.

### Generating a Nova project

Start a new project with:

```bash
rebar3 new nova my_first_nova
```

That will generate a Nova project for you.

```bash
rebar3 shell
```

This will fetch all dependencies and compile. After compilation it will start a shell that also say what port it is running on and some debug lines.

When the shell is started, open a browser and go to localhost:8080 and it will point to the my_first_nova server running Nova.


