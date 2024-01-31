# Rebar3 Nova #

If you have used the installation script, documented in the main README.md-file you should have a working installation of rebar3 nova. This gives you some handy commands to work with Nova;

`rebar3 new nova <project-name>` - Creates a new Nova project in the directory `<project-name>`

`rebar3 nova serve` - Starts a local webserver on port given in *sys.config*, serving the current project. This means that if something is changed in the project, the server will automatically reload the changed files.

`rebar3 nova routes` - Lists all routes in the current project (Includes included Nova applications aswell if they are properly configured)


We are hoping to increase the amount of commands that is available from the rebar3 nova command in the future.
