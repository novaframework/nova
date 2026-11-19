%%% The controllers of nova_runtime_plugin_SUITE.
-module(nova_runtime_plugin_pages).

-export([page/1]).

page(#{requests_before := Count}) ->
    {status, 200, #{}, integer_to_binary(Count)}.
