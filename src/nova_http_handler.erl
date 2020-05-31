%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling http requests
%%% @end

-module(nova_http_handler).

-export([
         init/2,
         handle/4
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type method() :: '_' | binary().
-type nova_http_state() :: #{mod := atom(),
                             func := atom(),
                             methods := [method()] | method(),
                             _ => _}.
-export_type([nova_http_state/0]).


%%--------------------------------------------------------------------
%% @doc
%% Callback function from nova_handler. This is the initial call where
%% all the logic is handed.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), State :: nova_http_state()) ->
                  {ok, Req1 :: cowboy_req:req(), State0 :: nova_http_state()}.
init(Req, State = #{mod := Mod, func := Func, methods := '_'}) ->
    {ok, StatusCode, Headers, Body, State0} = handle(Mod, Func, Req, State),
    Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
    {ok, Req1, State0};
init(Req = #{method := ReqMethod}, State = #{methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            init(Req, State#{methods := '_'});
        false ->
            Req1 =
                case nova_router:status_page(405, Req) of
                    {ok, StatusCode, Headers, Body, _} ->
                        cowboy_req:reply(StatusCode, Headers, Body, Req);
                    _ ->
                        cowboy_req:reply(405, #{}, <<>>, Req)
                end,
            {ok, Req1, State}
    end;
init(Req, State) ->
    case nova_router:status_page(404, Req) of
        {ok, StatusCode, Headers, Body, _} ->
            Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
            {ok, Req1, State};
        _ ->
            cowboy_req:reply(404, #{}, <<>>, Req)
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is exposed mostly cause we need to call it from nova_router.
%% It returns the raw handle request instead of the aggregated result returned in init/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(Mod :: atom(), Fun :: atom(), Req :: cowboy_req:req(), State :: nova_http_state()) ->
                    {ok, StatusCode :: integer(), Headers :: cowboy:http_headers(), Body :: binary(),
                     State0 :: nova_http_state()}.
handle(Mod, Fun, Req, State) ->
    ?DEBUG("Handling request for ~p:~p", [Mod, Fun]),
    Args =
        case maps:get(auth_data, State, undefined) of
            undefined ->
                [Req];
            SecObject ->
                [Req, SecObject]
        end,
    try erlang:apply(Mod, Fun, Args) of
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    Callback(RetObj, {Mod, Fun}, Req, State);
                _ ->
                    ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [RetObj, Mod, Fun]),
                    erlang:throw({unknown_handler_type, RetObj})
            end
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p",
                   [Mod, Fun, Type, Reason, Stacktrace]),
            case nova_router:status_page(500, Req) of
                {error, not_found} ->
                    %% Render our own view. Hide information if we're not in dev_mode
                    DevMode = nova:get_env(dev_mode, false),
                    {ok, HTML} = nova_internal_error_dtl:render([{module, Mod},
                                                                 {function, Fun},
                                                                 {type, Type},
                                                                 {reason, Reason},
                                                                 {stacktrace, Stacktrace},
                                                                 {dev_mode, DevMode}], []),
                    {ok, 500, #{}, HTML, State};
                Page ->
                    Page
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eunit functions         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
