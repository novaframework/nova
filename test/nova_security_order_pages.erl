%%% The controller, security callback and plugin of nova_security_order_SUITE.
-module(nova_security_order_pages).
-behaviour(nova_plugin).

-export([echo/1, by_header/1]).
-export([pre_security/4, pre_request/4, plugin_info/0]).

echo(#{auth_data := Saw} = Req) ->
    {json, 200, #{}, #{~"security_saw" => Saw,
                       ~"controller_json" => maps:get(json, Req, null),
                       ~"controller_qs" => maps:get(parsed_qs, Req, null)}}.

%% What the security callback found in the request, for the client to see
%% whether it was let in or not.
by_header(Req) ->
    Saw = #{~"body" => is_map_key(json, Req) orelse is_map_key(files, Req),
            ~"qs" => is_map_key(parsed_qs, Req),
            ~"early" => is_map_key(early, Req),
            ~"late" => is_map_key(late, Req)},
    case cowboy_req:header(~"authorization", Req) of
        ~"letmein" -> {true, Saw};
        _ -> {false, 401, #{~"x-security-saw" => iolist_to_binary(json:encode(Saw))}}
    end.

pre_security(_Req, _Env, #{early := crash}, _State) ->
    error(plugin_failed);
pre_security(Req, _Env, #{early := mark}, State) ->
    {ok, cowboy_req:set_resp_header(~"x-early", ~"ran", Req#{early => true}), State};
pre_security(Req, _Env, _Options, State) ->
    {ok, Req, State}.

pre_request(_Req, _Env, #{action := crash}, _State) ->
    error(plugin_failed);
pre_request(Req, _Env, #{action := stop}, State) ->
    {stop, {status, 403}, Req, State};
pre_request(Req, _Env, #{action := mark}, State) ->
    {ok, cowboy_req:set_resp_header(~"x-late", ~"ran", Req#{late => true}), State};
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_security_order_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Marks the request, crashes or stops, as its options say"}.
