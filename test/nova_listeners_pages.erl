%%% The controllers of nova_listeners_SUITE.
-module(nova_listeners_pages).

-export([where/1, metadata/1]).

where(Req) ->
    {status, 200, #{}, iolist_to_binary(io_lib:format("~p ~p", [nova_req:listener(Req), nova_req:http_version(Req)]))}.

%% Answers with the process that serves the request, and leaves logger
%% metadata of its own behind.
metadata(_Req) ->
    ok = logger:update_process_metadata(#{user => ~"alice"}),
    {status, 200, #{}, list_to_binary(pid_to_list(self()))}.
