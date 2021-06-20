-module(nova_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3
        ]).

-include_lib("nova/include/nova.hrl").

-callback init(Req, any()) -> {ok | module(), Req, any()}
                                  | {module(), Req, any(), any()}
                                  when Req::cowboy_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env = #{app := _App, module := Module, function := Function, controller_data := CtrlData}) ->
    State = CtrlData#{req => Req},
    try Module:Function(State) of
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    {ok, #{req := Req1} = State1} = Callback(RetObj, {Module, Function}, State),
                    StatusCode = maps:get(resp_status, State1, 200),
                    cowboy_req:reply(StatusCode, Req1),
                    {ok, Req1, State1};
                {Module, State} ->
                    Module:upgrade(Env, State);
                {Module, State, Options} -> %% Should we keep this one?
                    Module:upgrade(Env, State, Options);
                _ ->
                    ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [RetObj, Module, Function])
            end
    catch Class:Reason:Stacktrace ->
            terminate({crash, Class, Reason}, Req, Module),
            erlang:raise(Class, Reason, Stacktrace)
    end.

-spec terminate(any(), Req | undefined, module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, Module) ->
    case erlang:function_exported(Module, terminate, 3) of
        true ->
            Module:terminate(Reason, Req);
        false ->
            ok
    end.
