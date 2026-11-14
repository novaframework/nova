%%% The controllers, status pages and plugin of nova_status_page_SUITE. A
%%% request says with the x-404 and x-500 headers what the page for that
%%% status does.
-module(nova_status_page_pages).
-behaviour(nova_plugin).

-export([crash/1, throw_500/1, throw_404/1, bare_404/1, not_found/1, server_error/1]).
-export([post_request/4, plugin_info/0]).

crash(_Req) ->
    error(controller_failed).

throw_500(_Req) ->
    throw({500, not_a_map}).

throw_404(_Req) ->
    throw({404, gone}).

bare_404(_Req) ->
    {status, 404}.

not_found(Req) ->
    page(404, cowboy_req:header(~"x-404", Req, ~"ok")).

server_error(Req) ->
    page(500, cowboy_req:header(~"x-500", Req, ~"ok")).

page(Status, Behaviour) ->
    nova_status_page_SUITE:observed({page, Status}),
    case Behaviour of
        ~"ok" -> {status, Status, #{~"content-type" => ~"text/plain"}, [~"page ", integer_to_binary(Status)]};
        ~"crash" -> error(page_failed);
        ~"throw" -> throw({Status, page_failed})
    end.

post_request(Req, _Env, _Options, State) ->
    nova_status_page_SUITE:observed({post_request, maps:get(bindings, Req, undefined),
                                     maps:get(extra_state, Req, undefined)}),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_status_page_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Tells the suite which bindings and extra_state the request has after the controller"}.
