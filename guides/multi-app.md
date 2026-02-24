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

## Pragmatically starting other nova applications

### Starting an application

You can also start other nova applications pragmatically by calling `nova_sup:add_application/2` to add another nova application to your supervision tree. The routes will automatically be added to the routing-module.


## Stopping an application

To stop a nova application you can call `nova_sup:remove_application/1` with the name of the application you want to stop. Use this with caution since calling this method all routes for all other applications will be removed and re-added in order to filter out the one removed.
