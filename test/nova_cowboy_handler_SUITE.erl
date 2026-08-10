%%% Tests that plain cowboy handlers, as well as handlers using cowboy's
%%% sub-protocols (eg cowboy_loop), can be routed through nova with
%%% protocol => cowboy.
-module(nova_cowboy_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-define(PORT, 10080).

all() ->
    [regular_route_still_works,
     plain_cowboy_handler,
     loop_cowboy_handler,
     handler_arguments,
     streaming_handler,
     method_restriction].

init_per_suite(Config) ->
    application:load(nova),
    application:set_env(nova, bootstrap_application, base_app),
    application:set_env(nova, cowboy_configuration, #{port => ?PORT}),
    {ok, _} = application:ensure_all_started(nova),
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(nova),
    ok.

%% Sanity check - ordinary nova routes are unaffected.
regular_route_still_works(_Config) ->
    {200, Body} = request(get, "/base"),
    {ok, #{<<"app">> := <<"base">>}} = thoas:decode(list_to_binary(Body)),
    ok.

%% A basic cowboy handler replying directly from init/2.
plain_cowboy_handler(_Config) ->
    {200, "plain-ok"} = request(get, "/plain"),
    ok.

%% A cowboy_loop handler - the sub-protocol upgrade returned from
%% init/2 is honored.
loop_cowboy_handler(_Config) ->
    {200, "loop-ok"} = request(get, "/loop"),
    ok.

%% The arguments-option is passed as state to Handler:init/2.
handler_arguments(_Config) ->
    {200, "loop-args-ok"} = request(get, "/loop-args"),
    ok.

%% A handler streaming its response in several chunks with
%% cowboy_req:stream_reply/3 and cowboy_req:stream_body/3.
streaming_handler(_Config) ->
    {200, "chunk1chunk2chunk3done"} = request(get, "/stream"),
    ok.

%% The methods-option restricts cowboy-routes just like regular routes.
method_restriction(_Config) ->
    {405, _} = request(post, "/plain"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers            %%
%%%%%%%%%%%%%%%%%%%%%%%%

request(get, Path) ->
    do_request(get, {url(Path), []});
request(post, Path) ->
    do_request(post, {url(Path), [], "text/plain", ""}).

do_request(Method, Request) ->
    {ok, {{_, Status, _}, _Headers, Body}} = httpc:request(Method, Request, [], []),
    {Status, Body}.

url(Path) ->
    "http://127.0.0.1:" ++ integer_to_list(?PORT) ++ Path.
