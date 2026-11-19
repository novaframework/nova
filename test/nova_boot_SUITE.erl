%%% Nova either starts with its listener serving requests or does not start.
-module(nova_boot_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         restarted_supervisor_serves_again/1
        ]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     restarted_supervisor_serves_again
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

restarted_supervisor_serves_again(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    Supervisor = running(nova_sup),
    Monitor = monitor(process, Supervisor),
    exit(Supervisor, kill),
    receive {'DOWN', Monitor, process, Supervisor, killed} -> ok end,
    ?assert(stopped(nova, 50)),
    ?assertMatch({ok, _}, application:ensure_all_started(nova)),
    ?assertEqual({200, ~"page"}, nova_test_listener:get(Port, "/")),
    ?assertEqual([nova_listener], nova_sup:listeners()).

running(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> Pid
    end.

stopped(_Application, 0) ->
    false;
stopped(Application, Attempts) ->
    case lists:keymember(Application, 1, application:which_applications()) of
        false ->
            true;
        true ->
            timer:sleep(100),
            stopped(Application, Attempts - 1)
    end.

