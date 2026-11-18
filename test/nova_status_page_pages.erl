%%% The controllers, status pages and plugin of nova_status_page_SUITE. A
%%% request says with the x-404 and x-500 headers what the page for that
%%% status does.
-module(nova_status_page_pages).
-behaviour(nova_plugin).

-export([crash/1, throw_500/1, throw_404/1, bare_404/1, odd/1, crashing_security/1, not_found/1, server_error/1]).
-export([pre_request/4, post_request/4, plugin_info/0]).

crash(_Req) ->
    error(controller_failed).

throw_500(_Req) ->
    throw({500, not_a_map}).

throw_404(_Req) ->
    throw({404, gone}).

bare_404(_Req) ->
    {status, 404}.

odd(_Req) ->
    odd.

crashing_security(_Req) ->
    error(security_failed).

not_found(Req) ->
    page(404, nova_req:header(~"x-404", Req, ~"ok"), Req).

server_error(Req) ->
    page(500, nova_req:header(~"x-500", Req, ~"ok"), Req).

page(Status, Behaviour, Req) ->
    nova_test_observer:observed(nova_status_page_observer, {page, Status}),
    case Behaviour of
        ~"shape" ->
            Shape = {maps:get(resp_status_code, Req, none), maps:get(controller_data, Req, none),
                     is_map_key(crash_info, Req)},
            {status, Status, #{~"content-type" => ~"text/plain"}, io_lib:format("~0p", [Shape])};
        ~"ok" -> {status, Status, #{~"content-type" => ~"text/plain"}, [~"page ", integer_to_binary(Status)]};
        ~"crash" -> error(page_failed);
        ~"throw" -> throw({Status, page_failed})
    end.

pre_request(_Req, _Env, _Options, _State) ->
    error(plugin_failed).

post_request(Req, _Env, _Options, State) ->
    nova_test_observer:observed(nova_status_page_observer, {post_request, maps:get(bindings, Req, undefined),
                                     maps:get(extra_state, Req, undefined)}),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_status_page_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Tells the suite which bindings and extra_state the request has after the controller"}.
