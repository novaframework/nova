-module(nova_stream_h).
-moduledoc false.
-behavior(cowboy_stream).

-export([
         init/3,
         data/4,
         info/3,
         terminate/3,
         early_error/5
        ]).

-record(state, {
                next :: any(),
                req
               }).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
          -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    Req0 =
        case nova:get_env(use_sessions, true) of
            true ->
                case session_cookie(Req) of
                    present -> Req;
                    absent ->
                        {ok, SessionId} = nova_session:generate_session_id(),
                        ReqWithCookie = cowboy_req:set_resp_cookie(~"session_id", SessionId, Req),
                        ReqWithCookie#{nova_session_id => SessionId};
                    unparsable ->
                        {ok, SessionId} = nova_session:generate_session_id(),
                        Req#{nova_session_id => SessionId}
                end;
            _ ->
                Req
        end,
    %% Set the correct server-header information
    Req1 = cowboy_req:set_resp_header(<<"server">>, <<"Cowboy/Nova">>, Req0),
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{req = Req0, next = Next}}.

%% parse_cookies/1 exits on a malformed Cookie header, and Cowboy would then
%% log the whole request, Authorization included. Such a request gets a session
%% id but no cookie: the client may hold a valid session cookie Nova cannot
%% read, and a new one would overwrite it on every request.
session_cookie(Req) ->
    try lists:keymember(~"session_id", 1, cowboy_req:parse_cookies(Req)) of
        true -> present;
        false -> absent
    catch
        exit:{request_error, _, _} -> unparsable
    end.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
          -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

-spec info(cowboy_stream:streamid(), any(), State)
          -> {cowboy_stream:commands(), State} when State::state().
info(StreamID, {response, Code, _Headers, _Body} = Info, State = #state{next = Next})
  when is_integer(Code) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}};
info(StreamID, Info, State = #state{next = Next}) ->
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
