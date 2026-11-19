-module(nova_test_app_session_controller).

-export([
         set/1,
         set_with_status_text/1,
         set_and_stream/1,
         read/1,
         delete/1,
         untouched/1
        ]).

set(Req = #{bindings := #{~"value" := Value}}) ->
    ok = nova_session:set(Req, ~"value", Value),
    {json, #{stored => Value}}.

set_with_status_text(Req = #{bindings := #{~"value" := Value}}) ->
    ok = nova_session:set(Req, ~"value", Value),
    {status, ~"200 OK", #{}, ~"stored"}.

set_and_stream(Req0 = #{bindings := #{~"value" := Value}}) ->
    ok = nova_session:set(Req0, ~"value", Value),
    Req = cowboy_req:stream_reply(~"200 OK", #{~"content-type" => ~"text/plain"}, Req0),
    ok = cowboy_req:stream_body(~"stored", fin, Req),
    {status, 200, #{}, <<>>, Req}.

read(Req) ->
    case nova_session:get(Req, ~"value") of
        {ok, Value} -> {json, #{value => Value}};
        {error, not_found} -> {json, #{value => null}}
    end.

delete(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {json, 200, #{}, Req1, #{deleted => true}}.

untouched(_Req) ->
    {json, #{untouched => true}}.
