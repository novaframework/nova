# Building releases

Nova utilizes [relx](https://github.com/erlware/relx) for building releases and have a particular *profile* defined for creating such.

```
$ rebar3 as prod tar
```

This builds a release and archives it into a tar-file. As default erts is included in the release, but this can be configured through the options in `rebar.config`. To read more about release handling and configuration options we recommend reading relx-manual.
