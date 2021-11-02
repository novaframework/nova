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
execute(Req, Env = #{cowboy_handler := Handler, arguments := Arguments}) ->
    try Handler:init(Req, Arguments) of
        {ok, Req2, _State} ->
            Result = terminate(normal, Req2, Handler),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} ->
            Mod:upgrade(Req2, Env, Handler, State);
        {Mod, Req2, State, Opts} ->
            Mod:upgrade(Req2, Env, Handler, State, Opts)
    catch Class:Reason:Stacktrace ->
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            ?ERROR("Controller crashed (~p, ~p)~nStacktrace: ~p", [Class, Reason, Stacktrace]),
            render_response(Req#{crash_info => Payload}, Env, 500)
    end;
execute(Req, Env = #{module := Module, function := Function}) ->
    try Module:Function(Req) of
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    ?INFO("Called handler: ~p", [RetObj]),
                    {ok, Req0} = Callback(RetObj, {Module, Function}, Req),
                    render_response(Req0, Env);
                {error, not_found} ->
                    ?ERROR("Unknown return object1 ~p returned from module: ~p function: ~p",
                           [RetObj, Module, Function])
            end
    catch Class:Reason:Stacktrace ->
            ?ERROR("Controller crashed (~p, ~p)~nStacktrace: ~p", [Class, Reason, Stacktrace]),
            terminate(Reason, Req, Module),
            %% Build the payload object
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            render_response(Req#{crash_info => Payload}, Env, 500)
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

-spec render_response(Req :: cowboy_req:req(), Env :: map()) -> {ok, Req :: cowboy_req:req(), State :: map()}.
render_response(Req, Env) ->
    StatusCode = maps:get(resp_status_code, Req, 200),
    cowboy_req:reply(StatusCode, Req),
    {ok, Req, Env}.


render_response(Req, Env, StatusCode) ->
    case nova_router:lookup_url(StatusCode) of
        {error, _} ->
            %% Render the internal view of nova
            {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, {dummy, dummy}, Req),
            render_response(Req0, Env);
        {ok, _Bindings, A=#nova_handler_value{module = EMod, function = EFunc}} ->
            %% Show this view - how?
            ?INFO("Calling crashview: ~p", [A]),
            execute(Req, Env#{app => nova, module => EMod, function => EFunc})
    end.
