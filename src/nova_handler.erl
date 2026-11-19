-module(nova_handler).
-moduledoc false.
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3,
         request_error_status/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").

-callback init(Req, any()) -> {ok | module(), Req, any()}
                                  | {module(), Req, any(), any()}
                                  when Req::cowboy_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env = #{cowboy_handler := Handler, arguments := Arguments}) when is_atom(Handler) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try Handler:init(Req, Arguments) of
        {status, StatusCode, Req2, _State} when is_integer(StatusCode) ->
            render_response(Req2, maps:remove(cowboy_handler, Env), StatusCode);
        {ok, Req2, State} ->
            Result = call_terminate(Handler, normal, Req2, State),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} ->
            Mod:upgrade(Req2, Env, Handler, State);
        {Mod, Req2, State, Opts} ->
            Mod:upgrade(Req2, Env, Handler, State, Opts)
    catch
        exit:{request_error, Kind, _Message} ->
            render_request_error(Kind, Req, maps:remove(cowboy_handler, Env));
        Class:Reason0:Stacktrace when UseStacktrace == true ->
            Reason = nova_redact:term(Reason0),
            FormattedStacktrace = nova:format_stacktrace(Stacktrace),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500,
                                                              stacktrace => FormattedStacktrace}),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>, class => Class, reason => Reason, stacktrace => FormattedStacktrace}),
            render_response(Req#{crash_info => Payload}, maps:remove(cowboy_handler, Env), 500);
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500}),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>, class => Class, reason => Reason}),
            render_response(Req#{crash_info => Payload}, maps:remove(cowboy_handler, Env), 500)
    end;
execute(Req, Env = #{callback := _Callback}) ->
    execute_controller(Req, Env, maps:get(status_page, Env, false)).

-spec terminate(any(), Req :: cowboy_req:req() | undefined, function()) -> ok.
terminate(Reason, Req, Callback) ->
    {module, Module} = erlang:fun_info(Callback, module),
    call_terminate(Module, Reason, Req, #{}).

-doc false.
-spec request_error_status(Kind :: term()) -> 400 | 408 | 413.
request_error_status(timeout)           -> 408;
request_error_status(payload_too_large) -> 413;
request_error_status(_Kind)             -> 400.

call_terminate(Module, Reason, Req, State) ->
    case function_exported(Module, terminate, 3) of
        true ->
            Module:terminate(Reason, Req, State);
        false ->
            ok
    end.


execute_controller(Req, Env = #{callback := Callback}, StatusPage) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try RetObj = Callback(Req),
         call_handler(Callback, Req, RetObj, Env, false)
    of
        HandlerReturn ->
            HandlerReturn
    catch
        exit:{request_error, Kind, _Message} when StatusPage =:= false ->
            render_request_error(Kind, Req, Env);
        Class:{Status, Reason} when is_integer(Status), StatusPage =:= false ->
            %% This makes it so that we don't need to fetch the stacktrace
            ?LOG_ERROR(#{msg => <<"Controller threw an exception">>,
                         class => Class,
                         reason => nova_redact:term(Reason)}),
            case answered(Req) of
                false -> render_response(Req#{crash_info => nova_redact:term(Reason)}, Env, Status);
                {true, Req0} -> without_error_page(Req0, Env)
            end;
        Class:Reason0:Stacktrace when UseStacktrace == true ->
            Reason = nova_redact:term(Reason0),
            FormattedStacktrace = nova:format_stacktrace(Stacktrace),
            ?LOG_ERROR((crash_report(StatusPage))#{class => Class,
                                                   reason => Reason,
                                                   stacktrace => FormattedStacktrace}),
            terminate(Reason0, Req, Callback),
            %% Build the payload object
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500,
                                                              stacktrace => FormattedStacktrace}),
            render_crash(Req#{crash_info => Payload}, Env, StatusPage);
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            ?LOG_ERROR((crash_report(StatusPage))#{class => Class,
                                                   reason => Reason}),
            terminate(Reason0, Req, Callback),
            %% Build the payload object
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500}),
            render_crash(Req#{crash_info => Payload}, Env, StatusPage)
    end.

render_request_error(Kind, Req, Env) ->
    Status = request_error_status(Kind),
    ?LOG_WARNING(#{msg => ~"Request error in controller", kind => nova_redact:term(Kind), status_code => Status}),
    render_response(Req, Env, Status).

crash_report(false) ->
    #{msg => <<"Controller crashed">>};
crash_report(StatusPage) ->
    #{msg => ~"Status page crashed", status_page => StatusPage}.

render_crash(Req, Env, StatusPage) ->
    case answered(Req) of
        false -> render_crash_page(Req, Env, StatusPage);
        {true, Req0} -> without_error_page(Req0, Env)
    end.

render_crash_page(Req, Env = #{callback := Callback}, 500) ->
    case Callback =:= fun nova_error_controller:server_error/1 of
        true ->
            reply(500, maps:without([resp_body], Req), Env);
        false ->
            execute_controller(nova_router:status_page_request(500, #{}, Req),
                               Env#{app => nova, callback => fun nova_error_controller:server_error/1}, 500)
    end;
render_crash_page(Req, Env, _StatusPage) ->
    render_response(Req, Env, 500).

-spec answered(Req) -> false | {true, Req} when Req :: cowboy_req:req().
answered(Req) ->
    case response_begun(Req) of
        false -> false;
        Sent -> {true, Req#{has_sent_resp => Sent}}
    end.

response_begun(#{has_sent_resp := Sent}) ->
    Sent;
response_begun(#{nova_stream := true} = Req) ->
    Ref = make_ref(),
    cowboy_req:cast({nova_response_begun, self(), Ref}, Req),
    receive
        {Ref, Sent} -> Sent
    after 5000 ->
            false
    end;
response_begun(_Req) ->
    false.

without_error_page(Req, Env) ->
    ?LOG_WARNING(#{msg => ~"The response had begun when the controller failed, so no error page is sent"}),
    {ok, Req, Env}.

-spec execute_fallback(Callback :: function(), Req :: cowboy_req:req(), Response :: any(), Env :: map()) ->
          {ok, Req0 :: cowboy_req:req(), Env :: map()}.
execute_fallback(Callback, Req, Response, Env) ->
    {module, Module} = erlang:fun_info(Callback, module),
    Attributes = Module:module_info(attributes),
    case proplists:get_value(fallback_controller, Attributes, undefined) of
        undefined ->
            %% No fallback - render a crash-page
            Payload = #{status_code => 500,
                        status => <<"Problems with controller">>,
                        stacktrace => [{nova_handler, execute_fallback, 4}],
                        title => <<"Controller returned unsupported data">>,
                        extra_msg => list_to_binary(io_lib:format("Controller returned unsupported data: ~p",
                                                                  [nova_redact:term(Response)]))},
            render_response(Req#{crash_info => Payload}, Env, 500);
        [FallbackModule] ->
            case function_exported(FallbackModule, resolve, 2) of
                true ->
                    RetObj = FallbackModule:resolve(Req, Response),
                    call_handler(fun FallbackModule:resolve/1, Req, RetObj, Env, true);
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

-spec call_handler(Callback :: function(), Req :: cowboy_req:req(),
                   RetObj :: any(), Env :: map(), IsFallbackController :: boolean()) ->
          {ok, Req0 :: cowboy_req:req(), Env :: map()}.
call_handler(Callback, Req, RetObj, Env, IsFallbackController) ->
    case get_handler(RetObj) of
        {ok, HandlerCallback} ->
            {ok, Req0} = HandlerCallback(RetObj, Callback, Req),
            send_response(Req0, Env);
        {error, not_found} ->
            case IsFallbackController of
                true ->
                    {module, Module} = erlang:fun_info(Callback, module),
                    {name, Function} = erlang:fun_info(Callback, name),
                    ?LOG_ERROR(#{msg => <<"Controller returned unsupported result">>, controller => Module,
                                 function => Function, return => nova_redact:term(RetObj)}),
                    Payload = #{status_code => 500,
                                status => <<"Controller returned unsupported result">>,
                                title => <<"Error in controller">>,
                                extra_msg => list_to_binary(io_lib:format("Controller ~s:~s/1 returned a " ++
                                                                              "non-valid result ~p",
                                                                          [Module, Function, nova_redact:term(RetObj)]))},
                    render_response(Req#{crash_info => Payload}, Env, 500);
                _ ->
                    execute_fallback(Callback, Req, RetObj, Env)
            end
    end.

get_handler(RetObj) when is_tuple(RetObj), tuple_size(RetObj) > 0, is_atom(element(1, RetObj)) ->
    case nova_handlers:get_handler(element(1, RetObj)) of
        {ok, Callback} when is_function(Callback, 3) -> {ok, Callback};
        _ -> {error, not_found}
    end;
get_handler(_RetObj) ->
    {error, not_found}.

-spec send_response(Req :: cowboy_req:req(), Env :: map()) ->
          {ok, Req :: cowboy_req:req(), State :: map()}.
send_response(Req = #{resp_status_code := StatusCode}, Env) when is_integer(StatusCode); is_binary(StatusCode) ->
    reply(StatusCode, without_refused_body(StatusCode, Req), Env);
send_response(Req, Env) when not is_map_key(resp_status_code, Req) ->
    reply(200, Req, Env).

reply(_StatusCode, Req = #{has_sent_resp := _}, Env) ->
    {ok, Req, Env};
reply(StatusCode, Req, Env) ->
    {ok, cowboy_req:reply(StatusCode, Req), Env}.

without_refused_body(StatusCode, Req) when StatusCode =:= 204; StatusCode =:= 304 ->
    maps:remove(resp_body, Req);
without_refused_body(_StatusCode, Req) ->
    Req.

render_response(Req, Env, StatusCode) ->
    case application:get_env(nova, render_error_pages, true) of
        true ->
            case nova_router:lookup_url('_', StatusCode, '_', nova_router:dispatch_key(Env)) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, fun erlang:date/0, Req),
                    send_response(Req0, Env);
                {ok, _Bindings, #nova_handler_value{callback = Callback}} ->
                    execute_controller(nova_router:status_page_request(StatusCode, #{}, Req),
                                       Env#{app => nova, callback => Callback}, StatusCode)
            end;
        false ->
            {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, fun(_) -> ok end, Req),
            send_response(Req0, Env)
    end.

function_exported(Module, Function, Arity) ->
    Exports = Module:module_info(exports),
    case proplists:get_value(Function, Exports) of
        A when A == Arity ->
            true;
        _ ->
            false
    end.
