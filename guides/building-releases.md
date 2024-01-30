# Building releases

Nova utilizes [relx](https://github.com/erlware/relx) for building releases and has a particular *profile* defined for creating such. In a standard Nova-project there will be two profiles defined; `dev`and `prod`. The `dev`-profile is used for local development and the `prod`-profile is used for building releases.

So in order to build a release for production, you would run the following command:

```
$ rebar3 as prod tar
```

This builds a release and archives it into a tar-file. As default erts is included in the release, but this can be configured through the options in `rebar.config`. To read more about release handling and configuration options we recommend reading relx-manual.

If you are inexperienced with building releases we recommend reading the [relx-manual](https://www.rebar3.org/docs/releases#section-relx).
