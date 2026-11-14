-module(nova_handler).
-moduledoc false.
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3
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
execute(Req, Env = #{cowboy_handler := Handler, arguments := Arguments}) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try Handler:init(Req, Arguments) of
        {ok, Req2, State} ->
            Result = call_terminate(Handler, normal, Req2, State),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} ->
            Mod:upgrade(Req2, Env, Handler, State);
        {Mod, Req2, State, Opts} ->
            Mod:upgrade(Req2, Env, Handler, State, Opts)
    catch
        Class:Reason0:Stacktrace when UseStacktrace == true ->
            Reason = nova_redact:term(Reason0),
            FormattedStacktrace = nova:format_stacktrace(Stacktrace),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 404,
                                                              stacktrace => FormattedStacktrace}),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>, class => Class, reason => Reason, stacktrace => FormattedStacktrace}),
            render_response(Req#{crash_info => Payload}, maps:remove(cowboy_handler, Env), 404);
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 404}),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>, class => Class, reason => Reason}),
            render_response(Req#{crash_info => Payload}, maps:remove(cowboy_handler, Env), 404)
    end;
execute(Req, Env = #{callback := Callback}) ->
    %% Ensure that the module exists and have the correct function exported
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try RetObj = Callback(Req),
         call_handler(Callback, Req, RetObj, Env, false)
    of
        HandlerReturn ->
            HandlerReturn
    catch
        Class:{Status, Reason} when is_integer(Status) ->
            %% This makes it so that we don't need to fetch the stacktrace
            ?LOG_ERROR(#{msg => <<"Controller threw an exception">>,
                         class => Class,
                         reason => nova_redact:term(Reason)}),
            render_response(Req#{crash_info => nova_redact:term(Reason)}, Env, Status);
        Class:Reason0:Stacktrace when UseStacktrace == true ->
            Reason = nova_redact:term(Reason0),
            FormattedStacktrace = nova:format_stacktrace(Stacktrace),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>,
                         class => Class,
                         reason => Reason,
                         stacktrace => FormattedStacktrace}),
            terminate(Reason0, Req, Callback),
            %% Build the payload object
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500,
                                                              stacktrace => FormattedStacktrace}),
            render_response(Req#{crash_info => Payload}, Env, 500);
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>,
                         class => Class,
                         reason => Reason}),
            terminate(Reason0, Req, Callback),
            %% Build the payload object
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500}),
            render_response(Req#{crash_info => Payload}, Env, 500)
    end.

-spec terminate(any(), Req :: cowboy_req:req() | undefined, function()) -> ok.
terminate(Reason, Req, Callback) ->
    {module, Module} = erlang:fun_info(Callback, module),
    call_terminate(Module, Reason, Req, #{}).

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
            render_response(Req0, Env);
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
%% map) goes to the fallback controller like any other unknown return.
get_handler(RetObj) when is_tuple(RetObj), tuple_size(RetObj) > 0, is_atom(element(1, RetObj)) ->
    nova_handlers:get_handler(element(1, RetObj));
get_handler(_RetObj) ->
    {error, not_found}.

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
                    {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, fun erlang:date/0, Req),
                    render_response(Req0, Env);
                {ok, _Bindings, #nova_handler_value{callback = Callback}} ->
                    %% Recurse to the execute function to render error page
                    execute(Req, Env#{app => nova, callback => Callback})
            end;
        false ->
            {ok, Req0} = nova_basic_handler:handle_status({status, StatusCode}, fun(_) -> ok end, Req),
            render_response(Req0, Env)
    end.


function_exported(Module, Function, Arity) ->
    Exports = Module:module_info(exports),
    case proplists:get_value(Function, Exports) of
        A when A == Arity ->
            true;
        _ ->
            false
    end.
