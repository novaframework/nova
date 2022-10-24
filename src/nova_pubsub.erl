%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Pubsub system for Nova. It uses the pg/pg2 module.
%%%
%%% Pubsub subsystem is started with Nova and does not need any additional
%%% configuration. It uses the pg/pg2 module depending on the version of OTP.
%%% It provides a simple way of distributing messages to a large set of
%%% receivers and exposes a simple set of functions for doing that.
%%%
%%%
%%% A simple example of how to use pubsub in a ping/pong inspired game engine:
%%%
%%% -module(test_module).
%%% -export([player1/0,
%%%          player2/0,
%%%          start_game/0]).
%%%
%%% player1() ->
%%%   spawn(fun() ->
%%%     nova_pubsub:join(game_of_pong),
%%%     game_loop(1, "pong", "ping").
%%%
%%% player2() ->
%%%   spawn(fun() ->
%%%     nova_pubsub:join(game_of_pong),
%%%     game_loop(2, "ping", "pong").
%%%
%%% game_loop(Player, ExpectedMessage, Smash) ->
%%%   receive
%%%     ExpectedMessage ->
%%%       io:format("Player ~d received ~s and returning ~s~n", [Player, ExpectedMessage, Smash]),
%%%       nova_pubsub:broadcast(game_of_pong, "match1", Smash),
%%%       game_loop(Player, ExpectedMessage, Smash);
%%%     _ ->
%%%       game_loop(Player, ExpectedMessage, Smash)
%%%   end.
%%%
%%% @end
%%% Created :  8 Apr 2022 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_pubsub).
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

-include("../include/nova_pubsub.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Starts the pubsub subsystem. Only used by Nova internal supervisor!
%% @hidden
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Joining a channel with the calling process. Always returns ok
%% @end
%%--------------------------------------------------------------------
-spec join(Channel :: atom()) -> ok.
join(Channel) ->
    join(Channel, self()).

%%--------------------------------------------------------------------
%% @doc
%% Leaves a channnel. Will return ok on success and not_joined if the
%% calling process were not part of the channel.
%% @end
%%--------------------------------------------------------------------
-spec leave(Channel :: atom()) -> ok | not_joined.
leave(Channel) ->
    leave(Channel, self()).


%%--------------------------------------------------------------------
%% @doc
%% Same as join/1 but with a specified process.
%% @end
%%--------------------------------------------------------------------
-spec join(Channel :: atom(), Pid :: pid()) -> ok.
-if(?OTP_RELEASE >= 23).
join(Channel, Pid) when is_pid(Pid) ->
    pg:join(?SCOPE, Channel, Pid).
-else.
join(Channel, Pid) when is_pid(Pid) ->
    pg2:create({?SCOPE, Channel}), %% Create this to ensure it exists
    pg2:join({?SCOPE, Channel}, Pid).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Same as leave/1 but with a specified process.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Broadcasts a message to all members of a channel. Topic is specified
%% to differentiate messages within the same channel.
%% @end
%%--------------------------------------------------------------------
-spec broadcast(Channel :: atom(), Topic :: list() | binary(), Message :: any()) -> ok.
broadcast(Channel, Topic, Message) ->
    Members = get_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Works in the same way as broadcast/3 but only for members in the same
%% node.
%% @end
%%--------------------------------------------------------------------
-spec local_broadcast(Channel :: atom(), Topic :: list() | binary(), Message :: any()) -> ok.
local_broadcast(Channel, Topic, Message) ->
    Members = get_local_members(Channel),
    Envelope = create_envelope(Channel, self(), Topic, Message),
    [ Receiver ! Envelope || Receiver <- Members ],
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns all members for a given channel
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Works the same way as get_members/1 but returns only members on the
%% same node.
%% @end
%%--------------------------------------------------------------------
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
    #nova_pubsub{channel = Channel,
                 sender = Sender,
                 topic = Topic,
                 payload = Payload}.
