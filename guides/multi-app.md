# Including other nova applications

Nova is built to support inclusion of other applications built with Nova. To include an application you first need to include it in the `rebar.config` as a dependency. Then add the `nova_apps` option in your application-configuration. `nova_apps` should contain a list of atoms or two-tuples (For defining options).

## Configuration options

There's currently two different options available and they works in the same way in the routing-module.

| Key  | Value | Description |
|---|---|---|
| prefix | string | Defines if the applications urls should be prefixed |
| secure | false | {Mod, Fun} | Tells if the application should be secured |

## Example

*rebar.config*:

```
...
{deps, [
        {another_nova_app, "1.0.0"},
       ]
...


*sys.config*

```
...
{my_nova_app, [
  {nova_apps, [{another_nova_app, #{prefix => "/another"}}]}
  ]}
...
```
