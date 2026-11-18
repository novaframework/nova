-module(nova_handler).
-moduledoc false.
-behaviour(nova_pipeline).

%% Callbacks
-export([
         execute/2,
         terminate/3,
         request_error_status/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").

-callback init(Req, any()) -> {ok | websocket, Req, any()}
                                  | {websocket, Req, any(), nova_server:ws_opts()}
                                  | {status, 100..599, Req, any()}
                                  when Req::nova_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
                               when Req::nova_req:req(), Env::nova_server:env().
execute(Req, Env = #{cowboy_handler := Handler, arguments := Arguments}) when is_atom(Handler) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try Handler:init(Req, Arguments) of
        %% nova_ws_handler refusing the upgrade
        {status, StatusCode, Req2, _State} when is_integer(StatusCode) ->
            render_response(Req2, maps:remove(cowboy_handler, Env), StatusCode);
        {ok, Req2, State} ->
            Result = call_terminate(Handler, normal, Req2, State),
            {ok, Req2, Env#{result => Result}};
        {websocket, Req2, State} ->
            nova_req:upgrade_websocket(Req2, Env, Handler, State, #{});
        {websocket, Req2, State, Opts} ->
            nova_req:upgrade_websocket(Req2, Env, Handler, State, Opts)
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
    %% nova_router:render_status_page/5 says which status the page it put in
    %% place of the controller is for.
    execute_controller(Req, Env, maps:get(status_page, Env, false)).

-spec terminate(any(), Req :: nova_req:req() | undefined, function()) -> ok.
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


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% StatusPage is false for the controller of a route, or the status whose
%% page is being rendered.
execute_controller(Req, Env = #{callback := Callback}, StatusPage) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try run_controller(Callback, Req, Env, StatusPage =/= false) of
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

%% cowboy_req exits with request_error when the client sent something it
%% cannot parse or did not send the body in time. Cowboy answers that with a
%% 4xx when nothing catches it, so it is not a crash here either.
render_request_error(Kind, Req, Env) ->
    Status = request_error_status(Kind),
    ?LOG_WARNING(#{msg => ~"Request error in controller", kind => nova_redact:term(Kind), status_code => Status}),
    render_response(Req, Env, Status).

crash_report(false) ->
    #{msg => <<"Controller crashed">>};
crash_report(StatusPage) ->
    #{msg => ~"Status page crashed", status_page => StatusPage}.

%% A status page that fails is never looked up again, or a page that always
%% fails would be rendered without end. The page of another status hands
%% over to the 500 page, the 500 page of the application to Nova's own, and
%% Nova's own to a bare 500.
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

%% Whether the request has its answer, which is asked before an error page
%% is rendered for a controller that failed: the page would run for nothing,
%% and nova_stream_h would drop it. The request that comes back is marked as
%% answered.
-spec answered(Req) -> false | {true, Req} when Req :: nova_req:req().
answered(Req) ->
    case response_begun(Req) of
        false -> false;
        Sent -> {true, Req#{has_sent_resp => Sent}}
    end.

response_begun(Req) ->
    nova_req:response_begun(Req).

without_error_page(Req, Env) ->
    ?LOG_WARNING(#{msg => ~"The response had begun when the controller failed, so no error page is sent"}),
    {ok, Req, Env}.

%% {Status, Reason} raised in any class is how a controller asks execute/2 for
%% a status page, so it ends the span as a stop with that status. A status
%% page cannot ask for another one: whatever it raises is a crash.
run_controller(Callback, Req, Env, StatusPage) ->
    {module, Module} = erlang:fun_info(Callback, module),
    {name, Action} = erlang:fun_info(Callback, name),
    Metadata = request_metadata(Req),
    Span = nova_telemetry:start([nova, controller],
                                Metadata#{app => maps:get(app, Env, undefined),
                                          controller => Module,
                                          action => Action,
                                          route => maps:get(nova_route, Req, undefined),
                                          status_page => StatusPage}),
    try
        RetObj = Callback(Req),
        {ok, Req0, _Env0} = HandlerReturn = call_handler(Callback, Req, RetObj, Env, false),
        nova_telemetry:stop(Span, #{status => maps:get(resp_status_code, Req0, 200)}),
        HandlerReturn
    catch
        exit:{request_error, Kind, _} = Reason:Stacktrace when not StatusPage ->
            nova_telemetry:stop(Span, #{status => request_error_status(Kind)}),
            erlang:raise(exit, Reason, Stacktrace);
        Class:{Status, _} = Reason:Stacktrace when is_integer(Status), not StatusPage ->
            nova_telemetry:stop(Span, #{status => Status}),
            erlang:raise(Class, Reason, Stacktrace);
        Class:Reason:Stacktrace ->
            nova_telemetry:exception(Span, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% The raw path can hold a token and the peer is the client's address, so
%% those two are only exported when the application asks for them.
request_metadata(Req) ->
    #{Field => maps:get(Field, Req, undefined)
      || Field <- [method, host, scheme, version | opt_in_request_fields()]}.

opt_in_request_fields() ->
    case application:get_env(nova, telemetry_request_fields, []) of
        Fields when is_list(Fields) -> [Field || Field <- Fields, Field =:= path orelse Field =:= peer];
        _ -> []
    end.

-spec execute_fallback(Callback :: function(), Req :: nova_req:req(), Response :: any(), Env :: map()) ->
          {ok, Req0 :: nova_req:req(), Env :: map()}.
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

-spec call_handler(Callback :: function(), Req :: nova_req:req(),
                   RetObj :: any(), Env :: map(), IsFallbackController :: boolean()) ->
          {ok, Req0 :: nova_req:req(), Env :: map()}.
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

%% A controller that returns something other than a tagged tuple (an atom, a
%% map) goes to the fallback controller like any other unknown return, and
%% so does a tuple whose handler is not one for a request, as the handler of
%% `ws` is not.
get_handler(RetObj) when is_tuple(RetObj), tuple_size(RetObj) > 0, is_atom(element(1, RetObj)) ->
    case nova_handlers:get_handler(element(1, RetObj)) of
        {ok, Callback} when is_function(Callback, 3) -> {ok, Callback};
        _ -> {error, not_found}
    end;
get_handler(_RetObj) ->
    {error, not_found}.

-spec send_response(Req :: nova_req:req(), Env :: map()) ->
          {ok, Req :: nova_req:req(), State :: map()}.
send_response(Req = #{resp_status_code := StatusCode}, Env) when is_integer(StatusCode); is_binary(StatusCode) ->
    reply(StatusCode, without_refused_body(StatusCode, Req), Env);
send_response(Req, Env) when not is_map_key(resp_status_code, Req) ->
    reply(200, Req, Env).

%% Every response of a controller, a fallback controller or a status page
%% leaves from here. A handler or a controller that streamed its response,
%% or replied itself, hands back the request Cowboy marked as answered. When
%% it does not, nova_stream_h drops the second answer.
reply(_StatusCode, Req = #{has_sent_resp := _}, Env) ->
    {ok, Req, Env};
reply(StatusCode, Req, Env) ->
    {ok, nova_req:reply(StatusCode, Req), Env}.

%% A 204 and a 304 have no body, and Cowboy exits when given one.
without_refused_body(StatusCode, Req) when StatusCode =:= 204; StatusCode =:= 304 ->
    maps:remove(resp_body, Req);
without_refused_body(_StatusCode, Req) ->
    Req.

%% The status route of the application or, for a 404 and a 500 it has none
%% for, the page of Nova. A status nobody has a page for is sent bare.
render_response(Req, Env, StatusCode) ->
    case nova_router:lookup_url('_', StatusCode, '_', nova_router:dispatch_key(Env)) of
        {error, _} ->
            {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, fun erlang:date/0, Req),
            send_response(Req0, Env);
        {ok, _Bindings, #nova_handler_value{callback = Callback}} ->
            execute_controller(nova_router:status_page_request(StatusCode, #{}, Req),
                               Env#{app => nova, callback => Callback}, StatusCode)
    end.


function_exported(Module, Function, Arity) ->
    Exports = Module:module_info(exports),
    case proplists:get_value(Function, Exports) of
        A when A == Arity ->
            true;
        _ ->
            false
    end.
