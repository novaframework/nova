# My first home page

For more code visit [Nova blog](https://github.com/novaframework/nova_blog).

## Install templates

 * Install templates
 * Run template

```bash
rebar3 new nova my_page
===> Writing my_page/config/sys.config
===> Writing my_page/start.sh
===> Writing my_page/priv/my_page.routes.erl
===> Writing my_page/src/my_page.app.src
===> Writing my_page/src/my_page_app.erl
===> Writing my_page/src/my_page_sup.erl
===> Writing my_page/src/controllers/my_page_main_controller.erl
===> Writing my_page/rebar.config
===> Writing my_page/src/views/my_page_main.dtl
```

The structure here is in src we have two directories. One is for the controllers and one is for the views. In the views directory we have all the dtl files, the controllers handles what will be populated in them.

```html
<html>
<body>
<h1>{{message}}</h1>
</body>
</html>
```

Here we have the title ((message)) that will use the function in the controller. What we can see in the controller below we have a list with tuples that will be templated in where it find the id in the .dtl file.


```erlang
-module(my_page_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    {ok, [{message, "Nova is running!"}]}.
```

When we generated this project we also got a route file for my_page. It is in priv/my_page.routes.erl and when generated looks like this:

```erlang
#{prefix => "",
  type => html,
  security => false,
  routes => [
            {"/", my_page_main_controller, index}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
```

What this file say is that we use html, we don't have any security and when we point our web browser to "/" it will use my_page_main_controller module with the function index. As described above in my_page_main_controller.erl

## Start Nova

In the root directory run:

```bash
rebar3 shell
```

This will start a shell and compile all code. When Nova is started it will also compile all dtl files.

When the shell is up my_page is started at localhost:8080

It will be a page with the text "Nova is running!". Now we know that Nova works and we can start working with this.

## Change text in controllern

Set the project in dev mode:

Open a editor and open src/my_page.app.src and add {dev_mode, true} in env.

It should look like this:
```erlang
	{dev_mode, true},
        {web_port, 8080},
        {nova_applications, [
                               #{
                                 name => my_page,
                                 routes_file => "priv/my_page.routes.erl"
                                }
                              ]}

       ]},
```

Open a editor and open ./src/controller/my_page_main_controller.erl

Change the text "Nova is running!" to "This is my page!".

Reload the page and now the text have changed to "This is my page!". Now when we run in dev mode we use synch to auto-reload files. So when we update the browser now the text will be changed to "This is my page!".