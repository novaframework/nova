-module(nova_test_app_controller).

-export([
         index/1,
         json/1,
         echo/1,
         literal/1,
         method/1,
         redirect/1,
         teapot/1,
         crash/1,
         forbidden/1,
         extra/1,
         secret/1,
         auth_data/1,
         host/1,
         not_found/1
        ]).

index(_Req) ->
    {status, 200, #{<<"content-type">> => <<"text/plain">>}, <<"index">>}.

json(_Req) ->
    {json, #{ok => true, from => <<"nova_test_app">>}}.

echo(#{bindings := Bindings}) ->
    {json, Bindings}.

literal(_Req) ->
    {json, #{matched => <<"literal">>}}.

method(#{method := Method}) ->
    {json, #{method => Method}}.

redirect(_Req) ->
    {redirect, "/json"}.

teapot(_Req) ->
    {status, 418, #{}, <<"short and stout">>}.

crash(_Req) ->
    erlang:error(deliberate_crash).

forbidden(_Req) ->
    throw({403, not_allowed}).

extra(#{extra_state := ExtraState}) ->
    {json, #{answer => maps:get(answer, ExtraState, undefined)}}.

secret(_Req) ->
    {json, #{secret => true}}.

auth_data(#{auth_data := AuthData}) ->
    {json, AuthData}.

host(_Req) ->
    {json, #{host_scoped => true}}.

not_found(Req) ->
    Headers = case Req of
                  #{nova_route := Route} -> #{<<"x-nova-route">> => Route};
                  _ -> #{}
              end,
    {status, 404, Headers#{<<"content-type">> => <<"text/plain">>}, <<"custom not found">>}.
