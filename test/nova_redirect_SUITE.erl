%%% {redirect_local, Path} only redirects to a path on the site it is on.
-module(nova_redirect_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([local_path_is_redirected_to/1,
         other_sites_are_refused/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [local_path_is_redirected_to,
     other_sites_are_refused].

init_per_suite(Config) ->
    Routes = [#{prefix => "",
                plugins => [],
                routes => [{"/back", fun nova_redirect_pages:back/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

local_path_is_redirected_to(Config) ->
    {302, Headers, _Body} = back(Config, "%2Fdashboard%3Fx%3D1"),
    ?assertEqual(~"/dashboard?x=1", maps:get(~"location", Headers)).

%% //host and /\host name another host to a browser, which also drops a tab
%% before it reads the URL.
other_sites_are_refused(Config) ->
    [begin
         {{Status, Headers, _Body}, Events} = nova_test_log:collect(fun() -> back(Config, Next) end),
         ?assertEqual({Next, 400}, {Next, Status}),
         ?assertNot(is_map_key(~"location", Headers)),
         ?assertEqual([], nova_test_log:errors(Events))
     end || Next <- ["https%3A%2F%2Fevil.example.com", "%2F%2Fevil.example.com", "%2F%5Cevil.example.com",
                     "%2F%09%2Fevil.example.com", "evil.example.com", ""]].

back(Config, Next) ->
    nova_test_listener:response(?config(port, Config), "GET", ["/back?next=", Next], []).
