-module(nova_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3
        ]).

-include("../include/nova_logger.hrl").
-include("nova_router.hrl").

-callback init(Req, any()) -> {ok | module(), Req, any()}
                                  | {module(), Req, any(), any()}
                                  when Req::cowboy_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env = #{cowboy_handler := Handler, arguments := Arguments}) ->
    try erlang:apply(Handler, init, [Req, Arguments]) of
        {ok, Req2, _State} ->
            Result = terminate(normal, Req2, Handler),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} ->
            erlang:apply(Mod, upgrade, [Req2, Env, Handler, State]);
        {Mod, Req2, State, Opts} ->
            erlang:apply(Mod, upgrade, [Req2, Env, Handler, State, Opts])
    catch
        Class:Reason:Stacktrace ->
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            ?LOG_ERROR(#{msg => "Controller crashed", class => Class, reason => Reason, stacktrace => Stacktrace}),
            render_response(Req#{crash_info => Payload}, Env, 500)
    end;
execute(Req, Env = #{module := Module, function := Function}) ->
    %% Ensure that the module exists and have the correct function exported
    try erlang:apply(Module, Function, [Req]) of
        RetObj ->
            call_handler(Module, Function, Req, RetObj, Env, false)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => "Controller crashed",
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace}),
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
    case function_exported(Module, terminate, 3) of
        true ->
            erlang:apply(Module, terminate, [Reason, Req]);
        false ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%
-spec execute_fallback(Module :: atom(), Req :: cowboy_req:req(), Response :: any(), Env :: map()) ->
          {ok, Req0 :: cowboy_req:req(), Env :: map()}.
execute_fallback(Module, Req, Response, Env) ->
    Attributes = erlang:apply(Module, module_info, [attributes]),
    case proplists:get_value(fallback_controller, Attributes, undefined) of
        undefined ->
            %% No fallback - render a crash-page
            Payload = #{status_code => 500,
                        status => <<"Problems with controller">>,
                        stacktrace => [{nova_handler, execute_fallback, 4}],
                        title => <<"Controller returned unsupported data">>,
                        extra_msg => list_to_binary(io_lib:format("Controller returned unsupported data: ~p",
                                                                  [Response]))},
            render_response(Req#{crash_info => Payload}, Env, 500);
        [FallbackModule] ->
            case function_exported(FallbackModule, resolve, 2) of
                true ->
                    RetObj = erlang:apply(FallbackModule, resolve, [Req, Response]),
                    call_handler(FallbackModule, resolve, Req, RetObj, Env, true);
                _ ->
                    Payload = #{status_code => 500,
                                status => <<"Problems with fallback-controller">>,
                                title => <<"Fallback controller does not have a valid resolve/2 function">>,
                                extra_msg => list_to_binary(io_lib:format("Fallback controller ~s does not have "
                                                                          ++ "a valid resolve/2 function",
                                                                          [FallbackModule]))},
                    render_response(Req#{crash_info => Payload}, Env, 500)
            end
    end.

-spec call_handler(Module :: atom(), Function :: atom(), Req :: cowboy_req:req(),
                   RetObj :: any(), Env :: map(), IsFallbackController :: boolean()) ->
          {ok, Req0 :: cowboy_req:req(), Env :: map()}.
call_handler(Module, Function, Req, RetObj, Env, IsFallbackController) ->
    case nova_handlers:get_handler(element(1, RetObj)) of
        {ok, Callback} ->
            {ok, Req0} = Callback(RetObj, {Module, Function}, Req),
            render_response(Req0, Env);
        {error, not_found} ->
            case IsFallbackController of
                true ->
                    ?LOG_ERROR(#{msg => "Controller returned unsupported result", controller => Module,
                                 function => Function, return => RetObj}),
                    Payload = #{status_code => 500,
                                status => <<"Controller returned unsupported result">>,
                                title => <<"Error in controller">>,
                                extra_msg => list_to_binary(io_lib:format("Controller ~s:~s/1 returned a " ++
                                                                              "non-valid result ~s",
                                                                          [Module, Function, RetObj]))},
                    render_response(Req#{crash_info => Payload}, Env, 500);
                _ ->
                    execute_fallback(Module, Req, RetObj, Env)
            end
    end.

-spec render_response(Req :: cowboy_req:req(), Env :: map()) -> {ok, Req :: cowboy_req:req(), State :: map()}.
render_response(Req = #{resp_status_code := StatusCode}, Env) ->
    Req0 = cowboy_req:reply(StatusCode, Req),
    {ok, Req0, Env};
render_response(Req, Env) ->
    Req0 = cowboy_req:reply(200, Req),
    {ok, Req0, Env}.


render_response(Req, Env, StatusCode) ->
    case application:get_env(nova, render_error_pages, true) of
        true ->
            case nova_router:lookup_url(StatusCode) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, {dummy, dummy}, Req),
                    render_response(Req0, Env);
                {ok, _Bindings, #nova_handler_value{module = EMod, function = EFunc}} ->
                    %% Recurse to the execute function to render error page
                    execute(Req, Env#{app => nova, module => EMod, function => EFunc})
            end;
        false ->
            {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, {dummy, dummy}, Req),
            render_response(Req0, Env)
    end.


function_exported(Module, Function, Arity) ->
    Exports = erlang:apply(Module, module_info, [exports]),
    case proplists:get_value(Function, Exports) of
        A when A == Arity ->
            true;
        _ ->
            false
    end.
