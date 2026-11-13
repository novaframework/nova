-module(nova_recording_server).
-moduledoc false.

%% A nova_server for nova_server_SUITE: it runs on nova_server_cowboy and
%% notes each listener call in the public `nova_recording_server' ETS table,
%% so the suite sees how Nova drives a server that is not the default one.

-behaviour(nova_server).

-export([
         start_listener/3,
         stop_listener/1,
         drain_listeners/2,
         reply/4,
         inform/3,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         read_body/2,
         read_part/2,
         read_part_body/2,
         upgrade/5,
         upgrade/6
        ]).

start_listener(Ref, Configuration, Env) ->
    record({start_listener, Ref, Configuration}),
    nova_server_cowboy:start_listener(Ref, Configuration, Env).

stop_listener(Ref) ->
    record({stop_listener, Ref}),
    nova_server_cowboy:stop_listener(Ref).

drain_listeners(Refs, Timeout) ->
    record({drain_listeners, Refs, Timeout}),
    nova_server_cowboy:drain_listeners(Refs, Timeout).

reply(Status, Headers, Body, Req) -> nova_server_cowboy:reply(Status, Headers, Body, Req).
inform(Status, Headers, Req) -> nova_server_cowboy:inform(Status, Headers, Req).
stream_reply(Status, Headers, Req) -> nova_server_cowboy:stream_reply(Status, Headers, Req).
stream_body(Data, IsFin, Req) -> nova_server_cowboy:stream_body(Data, IsFin, Req).
stream_trailers(Trailers, Req) -> nova_server_cowboy:stream_trailers(Trailers, Req).
read_body(Req, Opts) -> nova_server_cowboy:read_body(Req, Opts).
read_part(Req, Opts) -> nova_server_cowboy:read_part(Req, Opts).
read_part_body(Req, Opts) -> nova_server_cowboy:read_part_body(Req, Opts).
upgrade(Mod, Req, Env, Handler, State) ->
    nova_server_cowboy:upgrade(Mod, Req, Env, Handler, State).
upgrade(Mod, Req, Env, Handler, State, Opts) ->
    nova_server_cowboy:upgrade(Mod, Req, Env, Handler, State, Opts).

record(Call) ->
    true = ets:insert(?MODULE, {erlang:unique_integer([monotonic]), Call}),
    ok.
