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


