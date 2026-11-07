-module(nova_test_app_session_controller).

-export([
         set/1,
         get/1,
         renew/1,
         delete/1,
         untouched/1
        ]).

set(Req = #{bindings := #{<<"value">> := Value}}) ->
    ok = nova_session:set(Req, ~"value", Value),
    {json, #{stored => Value}}.

get(Req) ->
    case nova_session:get(Req, ~"value") of
        {ok, Value} -> {json, #{value => Value}};
        {error, not_found} -> {json, #{value => null}}
    end.

renew(Req) ->
    {ok, Req1} = nova_session:renew(Req),
    ok = nova_session:set(Req1, ~"value", ~"renewed"),
    {json, 200, #{}, Req1, #{renewed => true}}.

delete(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {json, 200, #{}, Req1, #{deleted => true}}.

untouched(_Req) ->
    {json, #{untouched => true}}.
