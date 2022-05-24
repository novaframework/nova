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
-if(?OTP_RELEASE >= 23).
-spec start() -> ok.
start() ->
    pg:start(?SCOPE),
    ok.
-else.
start() ->
    pg2:start(),
    ok.
-endif.

%% Joining a channel
-spec join(Channel :: atom()) -> ok.
join(Channel) ->
    join(Channel, self()).

-spec leave(Channel :: atom()) -> ok | not_joined.
leave(Channel) ->
    leave(Channel, self()).

-spec join(Channel :: atom(), Pid :: pid()) -> ok.
-if(?OTP_RELEASE >= 23).
join(Channel, Pid) when is_pid(Pid) ->
    pg:join(?SCOPE, Channel, Pid).
-else.
join(Channel, Pid) when is_pid(Pid) ->
    pg2:create({?SCOPE, Channel}), %% Create this to ensure it exists
    pg2:join({?SCOPE, Channel}, Pid).
-endif.

-spec leave(Channel :: atom(), Pid :: pid()) -> ok | not_joined.
-if(?OTP_RELEASE >= 23).
leave(Channel, Pid) ->
    pg:leave(?SCOPE, Channel, Pid).
-else.
leave(Channel, Pid) ->
    case pg2:leave({?SCOPE, Channel}, Pid) of
        {error, _} ->
            not_joined;
        _ ->
            ok
    end.
-endif.

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
-if(?OTP_RELEASE >= 23).
get_members(Channel) ->
    pg:get_members(?SCOPE, Channel).
-else.
get_members(Channel) ->
    case pg2:get_members({?SCOPE, Channel}) of
        {error, _} ->
            [];
        Pidlist ->
            Pidlist
    end.
-endif.

-spec get_local_members(Channel :: atom()) -> [pid()].
-if(?OTP_RELEASE >= 23).
get_local_members(Channel) ->
    pg:get_local_members(?SCOPE, Channel).
-else.
get_local_members(Channel) ->
    case pg2:get_local_members({?SCOPE, Channel}) of
        {error, _} ->
            [];
        Pidlist ->
            Pidlist
    end.
-endif.

create_envelope(Channel, Sender, Topic, Payload) ->
    #nova_channel{channel = Channel,
                  sender = Sender,
                  topic = Topic,
                  payload = Payload}.
