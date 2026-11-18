-module(nova_stream_h).
-moduledoc false.
-behavior(cowboy_stream).
-include_lib("kernel/include/logger.hrl").

-export([
         init/3,
         data/4,
         info/3,
         terminate/3,
         early_error/5
        ]).

-record(state, {
                next :: {module(), term()} | undefined,
                req,
                %% Whether the response has begun: with its headers alone, when
                %% it is streamed, or in full
                sent = false :: false | headers | true,
                %% What Cowboy does with a response whose headers it refuses
                invalid_headers = error_terminate :: error_terminate | ignore,
                dropped = false :: boolean()
               }).

%% What is wrong with one request is wrong with every request to its route,
%% and this is logged from the connection process: one line a second on the
%% node, with the number of responses dropped since the line before.
-define(DROPPED_INTERVAL, 1000).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
          -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    %% Set the correct server-header information. The marker says that this
    %% handler can be asked whether a response began.
    Req1 = cowboy_req:set_resp_header(~"server", ~"Cowboy/Nova", Req#{nova_stream => true}),
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{req = Req, next = Next,
                      invalid_headers = invalid_headers(Opts)}}.

invalid_headers(#{invalid_response_headers := ignore}) -> ignore;
invalid_headers(_Opts)                                 -> error_terminate.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), State)
          -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

-spec info(cowboy_stream:streamid(), any(), State)
          -> {cowboy_stream:commands(), State} when State::state().
%% Nova asks before it answers: a controller that answered with cowboy_req
%% and did not hand back the request leaves it with the request from before.
info(_StreamID, {nova_response_begun, From, Ref}, State = #state{sent = Sent}) when is_pid(From) ->
    From ! {Ref, Sent},
    {[], State};
%% Cowboy stops the connection process, and every other request on it, when
%% a request is answered twice, so the second answer ends here.
info(_StreamID, {response, Code, _Headers, _Body}, State = #state{sent = Sent}) when Sent =/= false ->
    {[], dropped(Code, State)};
info(_StreamID, {headers, Code, _Headers}, State = #state{sent = Sent}) when Sent =/= false ->
    {[], dropped(Code, State)};
info(StreamID, Info, State) ->
    respond(StreamID, Info, State).

%% Cowboy takes a status as an integer or as a binary with the reason phrase.
respond(StreamID, {response, Code, Headers, _Body} = Response, State) when is_integer(Code); is_binary(Code) ->
    forward(StreamID, Response, begun(true, Headers, State));
respond(StreamID, {headers, Code, Headers} = Response, State) when is_integer(Code); is_binary(Code) ->
    forward(StreamID, Response, begun(headers, Headers, State));
respond(StreamID, Info, State) ->
    forward(StreamID, Info, State).

%% Cowboy sends nothing when it refuses the headers of a response. It
%% answers 500 through this handler instead, which has to get past.
begun(Sent, Headers, State = #state{invalid_headers = Mode}) ->
    case cowboy_http:validate_response_headers(Headers, #{invalid_response_headers => Mode}) of
        ok -> State#state{sent = Sent};
        error_terminate -> State
    end.

dropped(_Code, State = #state{dropped = true}) ->
    State;
dropped(Code, State = #state{req = Req}) ->
    case nova_once:per_interval({?MODULE, dropped}, ?DROPPED_INTERVAL) of
        {true, NotLogged} ->
            ?LOG_WARNING(#{msg => ~"The request was answered already, so this response is not sent. A controller, plugin or security callback that answers the request has to hand back the request it got from that",
                           status => Code, method => maps:get(method, Req, undefined),
                           path => maps:get(path, Req, undefined), not_logged => NotLogged});
        false ->
            ok
    end,
    State#state{dropped = true}.

forward(StreamID, Info, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), state()) -> any().
terminate(StreamID, Reason, #state{next = Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
                  cowboy_stream:partial_req(), Resp, cowboy:opts())
                 -> Resp
                        when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, {_, _Status, _Headers, _} = Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).
