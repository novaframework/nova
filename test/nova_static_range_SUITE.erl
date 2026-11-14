%%% Range requests for a static file answer what RFC 9110 section 14 asks
%%% for, whatever the client puts in the header.
-module(nova_static_range_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         no_range/1,
         first_and_last/1,
         open_ended/1,
         suffix/1,
         suffix_longer_than_the_file/1,
         last_past_the_end/1,
         first_past_the_end/1,
         empty_suffix/1,
         not_a_number/1,
         last_before_first/1,
         malformed/1,
         other_unit/1,
         several_ranges/1,
         relative_directory/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(CONTENT, ~"0123456789").
-define(RELATIVE_DIR, "nova_static_range_relative").

all() ->
    [
     no_range,
     first_and_last,
     open_ended,
     suffix,
     suffix_longer_than_the_file,
     last_past_the_end,
     first_past_the_end,
     empty_suffix,
     not_a_number,
     last_before_first,
     malformed,
     other_unit,
     several_ranges,
     relative_directory
    ].

init_per_suite(Config) ->
    Root = filename:join(?config(priv_dir, Config), "range_fixture"),
    ok = filelib:ensure_path(Root),
    ok = file:write_file(filename:join(Root, "ten.txt"), ?CONTENT),
    ok = filelib:ensure_path(?RELATIVE_DIR),
    ok = file:write_file(filename:join(?RELATIVE_DIR, "ten.txt"), ?CONTENT),
    Host = [#{prefix => "",
              routes => [
                         {"/files/[...]", Root},
                         {"/relative/[...]", ?RELATIVE_DIR}
                        ]}],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    file:del_dir_r(?RELATIVE_DIR).

no_range(Config) ->
    ?assertEqual({200, ?CONTENT}, nova_test_listener:get(?config(port, Config), "/files/ten.txt")).

first_and_last(Config) ->
    partial(Config, "bytes=0-3", ~"0123", ~"bytes 0-3/10"),
    partial(Config, "bytes=9-9", ~"9", ~"bytes 9-9/10").

open_ended(Config) ->
    partial(Config, "bytes=5-", ~"56789", ~"bytes 5-9/10").

suffix(Config) ->
    partial(Config, "bytes=-3", ~"789", ~"bytes 7-9/10").

suffix_longer_than_the_file(Config) ->
    partial(Config, "bytes=-50", ?CONTENT, ~"bytes 0-9/10").

last_past_the_end(Config) ->
    partial(Config, "bytes=5-100", ~"56789", ~"bytes 5-9/10").

first_past_the_end(Config) ->
    unsatisfiable(Config, "bytes=10-"),
    unsatisfiable(Config, "bytes=10-12").

empty_suffix(Config) ->
    unsatisfiable(Config, "bytes=-0").

not_a_number(Config) ->
    unsatisfiable(Config, "bytes=a-b"),
    unsatisfiable(Config, "bytes=1-b"),
    unsatisfiable(Config, "bytes=-b"),
    unsatisfiable(Config, "bytes=+1-3").

last_before_first(Config) ->
    unsatisfiable(Config, "bytes=5-2").

malformed(Config) ->
    unsatisfiable(Config, "bytes="),
    unsatisfiable(Config, "bytes=-"),
    unsatisfiable(Config, "bytes=5"),
    unsatisfiable(Config, "bytes=1-2-3"),
    unsatisfiable(Config, "bytes=--1").

other_unit(Config) ->
    whole(Config, "items=0-1"),
    whole(Config, "nonsense").

several_ranges(Config) ->
    whole(Config, "bytes=0-1,3-4").

relative_directory(Config) ->
    ?assertEqual({200, ?CONTENT}, nova_test_listener:get(?config(port, Config), "/relative/ten.txt")).

partial(Config, Range, Body, ContentRange) ->
    {Status, Headers, RespBody} = range_request(Config, Range),
    ?assertEqual({206, Body}, {Status, RespBody}),
    ?assertEqual(ContentRange, maps:get(~"content-range", Headers)).

unsatisfiable(Config, Range) ->
    {Status, Headers, _Body} = range_request(Config, Range),
    ?assertEqual({Range, 416}, {Range, Status}),
    ?assertEqual(~"bytes */10", maps:get(~"content-range", Headers)).

whole(Config, Range) ->
    {Status, Headers, Body} = range_request(Config, Range),
    ?assertEqual({Range, 200, ?CONTENT}, {Range, Status, Body}),
    ?assertNot(is_map_key(~"content-range", Headers)).

range_request(Config, Range) ->
    nova_test_listener:response(?config(port, Config), "GET", "/files/ten.txt", [{"range", Range}]).
