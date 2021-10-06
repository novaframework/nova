-module(nova_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3
        ]).

-include_lib("nova/include/nova.hrl").
-include("nova_router.hrl").

-callback init(Req, any()) -> {ok | module(), Req, any()}
                                  | {module(), Req, any(), any()}
                                  when Req::cowboy_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env = #{app := App, cowboy_handler := Handler, arguments := Arguments}) ->
    try Handler:init(Req, Arguments) of
        {ok, Req2, State} = R ->
            Result = terminate(normal, Req2, Handler),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} = R ->
            Mod:upgrade(Req2, Env, Handler, State);
        {Mod, Req2, State, Opts} = R ->
            Mod:upgrade(Req2, Env, Handler, State, Opts)
    catch Class:Reason:Stacktrace ->
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            case nova_router:lookup_url(500) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, State0} = nova_basic_handler:handle_ok({ok, Payload, #{view => nova_error}},
                                                                {dummy, dummy}, #{req => Req}),
                    render_response(State0);
                {ok, _Bindings, #nova_handler_value{module = EMod, function = EFunc}} ->
                    %% Show this view - how?
                    execute(Req, Env#{app => nova, module => EMod, function => EFunc, controller_data => Payload})
            end
    end;
execute(Req, Env = #{app := App, module := Module, function := Function, controller_data := CtrlData}) ->
    State = CtrlData#{req => Req},

    try Module:Function(State) of
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    {ok, State0} = Callback(RetObj, {Module, Function}, State),
                    render_response(State0);
                {error, not_found} ->
                    ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [RetObj, Module, Function])
            end
    catch Class:Reason:Stacktrace ->
            terminate(Reason, Req, Module),
            %% Build the payload object
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},

            case nova_router:lookup_url(500) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, State0} = nova_basic_handler:handle_ok({ok, Payload, #{view => nova_error}},
                                                                {dummy, dummy}, State),
                    render_response(State0);
                {ok, _Bindings, #nova_handler_value{module = EMod, function = EFunc}} ->
                    %% Show this view - how?
                    execute(Req, Env#{app => nova, module => EMod, function => EFunc, controller_data => Payload})
            end
    end.

-spec terminate(any(), Req | undefined, module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, Module) ->
    case erlang:function_exported(Module, terminate, 3) of
        true ->
            Module:terminate(Reason, Req);
        false ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec render_response(State :: map()) -> {ok, Req :: cowboy_req:req(), State0 :: map()}.
render_response(#{req := Req} = State) ->
    StatusCode = maps:get(resp_status, State, 200),
    cowboy_req:reply(StatusCode, Req),
    {ok, Req, State}.
