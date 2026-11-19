%%% The controllers, security callback and plugin of nova_upload_cleanup_SUITE.
-module(nova_upload_cleanup_pages).
-behaviour(nova_plugin).

-export([leave/1, keep/1, move/1, crash/1, refuse/1]).
-export([pre_request/4, plugin_info/0]).

leave(#{files := Files}) ->
    Sizes = [filelib:file_size(Path) || #{result := #{path := Path}} <- Files],
    {json, 200, #{}, #{sizes => Sizes}}.

keep(#{files := [#{result := #{path := Path}} | _]}) ->
    ok = nova_multipart_spool:keep(Path),
    {status, 200, #{}, ~"kept"}.

move(#{files := [#{result := #{path := Path}}]}) ->
    ok = file:rename(Path, <<Path/binary, ".moved">>),
    {status, 200, #{}, ~"moved"}.

crash(_Req) ->
    error(controller_failed).

refuse(_Req) ->
    false.

pre_request(Req, _Env, _Options, State) ->
    {stop, {status, 403}, Req, State}.

plugin_info() ->
    #{title => ~"nova_upload_cleanup_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Stops every request with a 403"}.
