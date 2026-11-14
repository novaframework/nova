%%% The controllers of nova_catch_all_SUITE.
-module(nova_catch_all_pages).

-export([extra_state/1, pathinfo/1]).

extra_state(#{extra_state := ExtraState}) ->
    {status, 200, #{}, io_lib:format("~0p", [ExtraState])}.

pathinfo(#{extra_state := #{pathinfo := Pathinfo}}) ->
    {status, 200, #{}, lists:join(~"|", Pathinfo)};
pathinfo(_Req) ->
    {status, 200, #{}, ~"no pathinfo"}.
