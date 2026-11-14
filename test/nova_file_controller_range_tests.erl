%%% The numbers of a Range header, with more digits than a listener lets
%%% through by default. HTTP/2 carries about 32 kB of header in one request.
-module(nova_file_controller_range_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CONTENT, ~"0123456789").
-define(MANY_DIGITS, 60000).
-define(BUDGET_MS, 50).

setup() ->
    Dir = filename:absname(filename:join(["_build", "test", "tmp", atom_to_list(?MODULE)])),
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(filename:join(Dir, "ten.txt"), ?CONTENT),
    Dir.

cleanup(Dir) ->
    ok = file:del_dir_r(Dir).

range(Dir, RangeSpec) ->
    Result = nova_file_controller:get_file(#{extra_state => #{static => {file, filename:join(Dir, "ten.txt")},
                                                              options => #{}},
                                             headers => #{~"range" => iolist_to_binary(["bytes=", RangeSpec])}}),
    case Result of
        {sendfile, 206, #{~"content-range" := ContentRange}, _File, _Mime} -> {206, ContentRange};
        {sendfile, 200, _Headers, _File, _Mime} -> 200;
        {status, 416, _Headers} -> 416
    end.

%% The fastest of a few runs, so a busy machine does not fail the test.
milliseconds(Request) ->
    lists:min([elapsed(Request) || _Run <- lists:seq(1, 5)]).

elapsed(Request) ->
    Start = erlang:monotonic_time(millisecond),
    _ = Request(),
    erlang:monotonic_time(millisecond) - Start.

many(Digit) ->
    binary:copy(Digit, ?MANY_DIGITS).

range_number_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
             Whole = {206, ~"bytes 0-9/10"},
             [{"a first position of many digits is past the end, without the time it takes to build the number",
               fun() ->
                       Range = [many(~"1"), "-"],
                       ?assertEqual(416, range(Dir, Range)),
                       ?assert(milliseconds(fun() -> range(Dir, Range) end) < ?BUDGET_MS)
               end},
              {"a last position of many digits is the end of the file",
               fun() ->
                       Range = ["0-", many(~"9")],
                       ?assertEqual(Whole, range(Dir, Range)),
                       ?assert(milliseconds(fun() -> range(Dir, Range) end) < ?BUDGET_MS)
               end},
              {"a suffix of many digits is the whole file",
               fun() ->
                       Range = ["-", many(~"7")],
                       ?assertEqual(Whole, range(Dir, Range)),
                       ?assert(milliseconds(fun() -> range(Dir, Range) end) < ?BUDGET_MS)
               end},
              {"many leading zeros do not make a number large",
               fun() ->
                       Range = [many(~"0"), "5-", many(~"0"), "7"],
                       ?assertEqual({206, ~"bytes 5-7/10"}, range(Dir, Range)),
                       ?assert(milliseconds(fun() -> range(Dir, Range) end) < ?BUDGET_MS)
               end},
              {"many digits with something else among them are not a range",
               ?_assertEqual(200, range(Dir, [many(~"1"), "x-"]))},

              {"19 digits are read as the number they are",
               [?_assertEqual(416, range(Dir, "9999999999999999999-")),
                ?_assertEqual(Whole, range(Dir, "0-9999999999999999999")),
                ?_assertEqual(Whole, range(Dir, "-9999999999999999999")),
                ?_assertEqual(416, range(Dir, "9999999999999999998-9999999999999999999")),
                ?_assertEqual(200, range(Dir, "9999999999999999999-9999999999999999998"))]},
              {"20 digits are larger than any file",
               [?_assertEqual(416, range(Dir, "10000000000000000000-")),
                ?_assertEqual(Whole, range(Dir, "0-10000000000000000000")),
                ?_assertEqual(Whole, range(Dir, "-10000000000000000000")),
                ?_assertEqual(416, range(Dir, "9999999999999999999-10000000000000000000")),
                ?_assertEqual(200, range(Dir, "10000000000000000000-9999999999999999999")),
                ?_assertEqual(416, range(Dir, "10000000000000000000-10000000000000000000"))]},
              {"leading zeros do not count as digits",
               [?_assertEqual({206, ~"bytes 5-7/10"}, range(Dir, "0005-0007")),
                ?_assertEqual({206, ~"bytes 7-9/10"}, range(Dir, "-0003")),
                ?_assertEqual(Whole, range(Dir, "000-")),
                ?_assertEqual({206, ~"bytes 5-9/10"}, range(Dir, "00000000000000000005-")),
                ?_assertEqual({206, ~"bytes 0-0/10"}, range(Dir, "0-00000000000000000000")),
                ?_assertEqual(416, range(Dir, "-000"))]},
              {"no digits are not a number",
               [?_assertEqual(200, range(Dir, "-")),
                ?_assertEqual(200, range(Dir, "")),
                ?_assertEqual(Whole, range(Dir, "0-")),
                ?_assertEqual({206, ~"bytes 9-9/10"}, range(Dir, "-1"))]}]
     end}.
