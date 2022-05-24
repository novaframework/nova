-module(nova_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([
         execute/2,
         terminate/3
        ]).

-include("../include/nova_comp.hrl").
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
        ?STACKTRACE(Class, Reason, Stacktrace)
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            ?LOG_ERROR(#{msg => "Controller crashed", class => Class, reason => Reason, stacktrace => Stacktrace}),
            render_response(Req#{crash_info => Payload}, Env, 500)
    end;
execute(Req, Env = #{module := Module, function := Function}) ->
    %% Ensure that the module exists and have the correct function exported
    case lists:search(fun({Export, Arity}) -> Export == Function andalso Arity == 1 end,
                      erlang:apply(Module, module_info, [exports])) of
        {value, _} ->
            try erlang:apply(Module, Function, [Req]) of
                RetObj ->
                    case nova_handlers:get_handler(element(1, RetObj)) of
                        {ok, Callback} ->
                            {ok, Req0} = Callback(RetObj, {Module, Function}, Req),
                            render_response(Req0, Env);
                        {error, not_found} ->
                            ?LOG_ERROR(#{msg => "Controller returned unsupported result", controller => Module,
                                         function => Function, return => RetObj})
                    end
            catch
                ?STACKTRACE(Class, Reason, Stacktrace)
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
            end;
        _ ->
            ?LOG_ERROR(#{msg => "Could not find controller",
                         controller => Module,
                         function => io_lib:format("~s/1", [Function])}),
            Payload = #{status_code => 500,
                        status => "Problems with application",
                        stacktrace => [{Module, Function, 1}],
                        title => "Woops. It seems like you have a problem with your application!",
                        extra_msg => "Nova could not find your controller:function.
                                      Pleae check your spelling and/or that the module" ++
                                     " have been properly loaded. "
                       },
            render_response(Req#{crash_info => Payload}, Env, 500)
    end.

-spec terminate(any(), Req | undefined, module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, Module) ->
    case erlang:function_exported(Module, terminate, 3) of
        true ->
            erlang:apply(Module, terminate, [Reason, Req]);
        false ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%
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
