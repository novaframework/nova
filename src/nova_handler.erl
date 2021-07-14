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
execute(Req, Env = #{app := App, module := Module, function := Function, controller_data := CtrlData,
                     extra_state := ExtraState}) ->
    State = CtrlData#{req => Req},

    %% Lazy load evaluation of the case expression
    ControllerInvokation = fun() ->
                                   case App of
                                       cowboy ->
                                           {cowboy, Module:init(Req, ExtraState)};
                                       _ ->
                                           Module:Function(State)
                                   end
                           end,

    try ControllerInvokation() of
        {cowboy, {ok, Req0, _State0}} ->
            Result = terminate(normal, Req0, Module),
            {ok, Req0, Env#{result => Result}};
        {cowboy, {Mod, Req0, CowboyState}} ->
            Mod:upgrade(Req0, Env, Module, CowboyState);
        {cowboy, {Mod, Req0, CowboyState, Opts}} ->
            Mod:upgrade(Req0, Env, Module, CowboyState, Opts);
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    {ok, State0} = Callback(RetObj, {Module, Function}, State),
                    render_response(State0);
                {Module, State} ->
                    Module:upgrade(Env, State);
                {Module, State, Options} -> %% Should we keep this one?
                    Module:upgrade(Env, State, Options);
                _ ->
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
                {ok, #mmf{module = EMod, function = EFunc}, _PathState} ->
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

-spec render_response(State :: nova_http:http_state()) -> {ok, Req :: cowboy_req:req(), State :: nova_http:state()}.
render_response(#{req := Req} = State) ->
    StatusCode = maps:get(resp_status, State, 200),
    cowboy_req:reply(StatusCode, Req),
    {ok, Req, State}.
