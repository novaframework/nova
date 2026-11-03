%%%-------------------------------------------------------------------
%%% Created :  8 Apr 2022 by Niclas Axelsson <niclas@burbas.se>
-module(nova_pubsub).
-moduledoc """
Pubsub system for Nova, built on OTP's `pg` module (in the `nova_scope` scope).

Pubsub subsystem is started with Nova and does not need any additional configuration. It provides a simple way of distributing messages to a large set of receivers, on this node or across a cluster, and exposes a simple set of functions for doing that.

Members receive a `#nova_pubsub{}` record (from `nova_pubsub.hrl`) with the channel, sender, topic and payload. A simple example of how to use pubsub in a ping/pong inspired game engine:

```erlang
-module(test_module).
-export([player/3]).

-include_lib("nova/include/nova_pubsub.hrl").
-include_lib("kernel/include/logger.hrl").

player(Name, Expected, Smash) ->
    spawn(fun() ->
                  ok = nova_pubsub:join(game_of_pong),
                  game_loop(Name, Expected, Smash)
          end).

game_loop(Name, Expected, Smash) ->
    receive
        #nova_pubsub{topic = "match1", payload = Expected} ->
            ?LOG_INFO(#{player => Name, received => Expected, returning => Smash}),
            nova_pubsub:broadcast(game_of_pong, "match1", Smash),
            game_loop(Name, Expected, Smash);
        _ ->
            game_loop(Name, Expected, Smash)
    end.
```
""".
-export([
         start/0,
         join/1,
         join/2,
         leave/1,
         leave/2,
         broadcast/3,
         local_broadcast/3,
         get_members/1,
         get_local_members/1
        ]).

-export_type([channel/0]).

-type channel() :: atom() | binary().
%% A pubsub channel. Backed by `pg', so any atom or binary works; binaries
%% are useful for dynamic, per-entity channels (e.g. per-tenant) without
%% growing the atom table.

-define(SCOPE, nova_scope).

-include("../include/nova_pubsub.hrl").

%%--------------------------------------------------------------------
%% Starts the pubsub subsystem. Only used by Nova internal supervisor!
-doc false.
-spec start() -> ok.
start() ->
    pg:start(?SCOPE),
    ok.

%%--------------------------------------------------------------------
-doc "Joining a channel with the calling process. Always returns ok".
-spec join(Channel :: channel()) -> ok.
join(Channel) ->
    join(Channel, self()).

%%--------------------------------------------------------------------
-doc "Leaves a channnel. Will return ok on success and not_joined if the calling process were not part of the channel.".
-spec leave(Channel :: channel()) -> ok | not_joined.
leave(Channel) ->
    leave(Channel, self()).


%%--------------------------------------------------------------------
-doc "Same as join/1 but with a specified process.".
-spec join(Channel :: channel(), Pid :: pid()) -> ok.
join(Channel, Pid) when is_pid(Pid) ->
    pg:join(?SCOPE, Channel, Pid).

%%--------------------------------------------------------------------
-doc "Same as leave/1 but with a specified process.".
-spec leave(Channel :: channel(), Pid :: pid()) -> ok | not_joined.
leave(Channel, Pid) ->
    pg:leave(?SCOPE, Channel, Pid).

%%--------------------------------------------------------------------
-doc "Broadcasts a message to all members of a channel. Topic is specified to differentiate messages within the same channel.".
-spec broadcast(Channel :: channel(), Topic :: list() | binary(), Message :: any()) -> ok.
broadcast(Channel, Topic, Message) ->
    Members = get_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.


%%--------------------------------------------------------------------
-doc "Works in the same way as broadcast/3 but only for members in the same node.".
-spec local_broadcast(Channel :: channel(), Topic :: list() | binary(), Message :: any()) -> ok.
local_broadcast(Channel, Topic, Message) ->
    Members = get_local_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.


%%--------------------------------------------------------------------
-doc "Returns all members for a given channel".
-spec get_members(Channel :: channel()) -> [pid()].
get_members(Channel) ->
    pg:get_members(?SCOPE, Channel).

%%--------------------------------------------------------------------
-doc "Works the same way as get_members/1 but returns only members on the same node.".
-spec get_local_members(Channel :: channel()) -> [pid()].
get_local_members(Channel) ->
    pg:get_local_members(?SCOPE, Channel).

create_envelope(Channel, Sender, Topic, Payload) ->
    #nova_pubsub{channel = Channel,
                 sender = Sender,
                 topic = Topic,
                 payload = Payload}.
