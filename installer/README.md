## Rebar3 templates for nova

### Installation

#### Use the installation script

Just run the install.sh-script:
```
$ ./install.sh
```

#### Manual installation

Copy the files found in this repository to `~/.config/rebar3/templates`. If the directory does not exist just create it.


### Usage

Just tell rebar3 to create a new project using the Nova-template. This will create a directory with the same name as the project.
```
$ rebar3 new nova my_project
```

Now you can fire up the whole thing and start developing fancy web applications in Erlang
