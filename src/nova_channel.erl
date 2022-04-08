%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Channels is a pubsub system for Nova. It uses the `pg` module.
%%% @end
%%% Created :  8 Apr 2022 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_channel).
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

-define(SCOPE, nova_scope).

-include("../include/nova_channel.hrl").


%% Start scope
-spec start() -> ok.
start() ->
    pg:start(?SCOPE),
    ok.

%% Joining a channel
-spec join(Channel :: atom()) -> ok.
join(Channel) ->
    join(Channel, self()).

-spec join(Channel :: atom(), Pid :: pid()) -> ok.
join(Channel, Pid) when is_pid(Pid) ->
    pg:join(?SCOPE, Channel, Pid).

-spec leave(Channel :: atom()) -> ok | not_joined.
leave(Channel) ->
    leave(Channel, self()).

-spec leave(Channel :: atom(), Pid :: pid()) -> ok | not_joined.
leave(Channel, Pid) ->
    pg:leave(?SCOPE, Channel, Pid).

%% Broadcast to members
-spec broadcast(Channel :: atom(), Topic :: list() | binary(), Message :: any()) -> ok.
broadcast(Channel, Topic, Message) ->
    Members = get_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.

%% Broadcast to processes located on the same node
-spec local_broadcast(Channel :: atom(), Topic :: list() | binary(), Message :: any()) -> ok.
local_broadcast(Channel, Topic, Message) ->
    Members = get_local_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.

-spec get_members(Channel :: atom()) -> [pid()].
get_members(Channel) ->
    pg:get_members(?SCOPE, Channel).

-spec get_local_members(Channel :: atom()) -> [pid()].
get_local_members(Channel) ->
    pg:get_local_members(?SCOPE, Channel).


create_envelope(Channel, Sender, Topic, Payload) ->
    #nova_channel{channel = Channel,
                  sender = Sender,
                  topic = Topic,
                  payload = Payload}.
