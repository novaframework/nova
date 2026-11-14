%%% The controller, security callback and plugin of nova_security_order_SUITE.
-module(nova_security_order_pages).
-behaviour(nova_plugin).

-export([echo/1, by_header/1]).
-export([pre_request/4, plugin_info/0]).

echo(#{auth_data := #{saw_body := SawBody}} = Req) ->
    {json, 200, #{}, #{~"security_saw_body" => SawBody,
                       ~"controller_json" => maps:get(json, Req, null)}}.

by_header(Req) ->
    SawBody = is_map_key(json, Req),
    case cowboy_req:header(~"authorization", Req) of
        ~"letmein" -> {true, #{saw_body => SawBody}};
        _ -> {false, 401, #{~"x-security-saw-body" => atom_to_binary(SawBody)}}
    end.

pre_request(_Req, _Env, #{action := crash}, _State) ->
    error(plugin_failed);
pre_request(Req, _Env, #{action := stop}, State) ->
    {stop, {status, 403}, Req, State}.

plugin_info() ->
    #{title => ~"nova_security_order_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Crashes or stops, as its options say"}.
