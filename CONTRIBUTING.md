# Contributing to Nova

Contributions are welcome. This page says how to build and test Nova, what a
pull request needs and what happens when it is merged. Taking part is
covered by the [code of conduct](CODE_OF_CONDUCT.md).

## Building and testing

Nova needs Erlang/OTP 27 or later and `rebar3`. CI builds with OTP 27, 28
and 29.

```shell
rebar3 compile
rebar3 eunit
rebar3 ct
```

The Common Test suites start real listeners on free ports. Their logs are
written to `_build/test/logs`.

## Static checks

CI runs these for every pull request, next to the tests:

```shell
rebar3 dialyzer
rebar3 xref
rebar3 ex_doc
```

Run them before opening a pull request. `rebar3 ex_doc` builds the guides in
`guides/` and the API reference, and warns about links and references that
lead nowhere.

## Making a change

* Branch from a fresh `master`.
* Add or update tests with every change: EUnit for a module, Common Test
  for behaviour that is seen over HTTP or a websocket.
* Update the guide that describes the behaviour you change. A change that
  an application has to act on when it upgrades gets a row in
  `guides/upgrading.md` and an entry in `guides/deprecations.md`.
* Do not edit `CHANGELOG.md` by hand. It is written by
  [git-cliff](https://git-cliff.org) from the commit messages, with the
  configuration in `cliff.toml`.

## Commits and pull requests

Write commit messages and pull request titles as
[conventional commits](https://www.conventionalcommits.org):
`fix: answer a view without a template with a 500`,
`feat(session): add set_provisional/3`.

A pull request is squashed into one commit on `master`, and the title of the
pull request is the message of that commit. When the build of `master` has
passed, a release is made from it. The title decides the version:

| Title starts with | Version bump |
|---|---|
| `feat:` or `feat(scope):` | Minor |
| `fix:` or `fix(scope):` | Patch |
| A type followed by `!`, such as `feat!:` | Major |
| Anything else | Patch |

Choose the type for what the change means to an application that uses Nova,
and keep the title to what changed.

## Security issues

Do not report a vulnerability in an issue or a pull request. The
[security policy](SECURITY.md) says how to report one privately.

## Licence

Nova is licensed under the [Apache License 2.0](LICENSE). Contributions are
accepted under the same licence.
