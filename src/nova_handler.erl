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
execute(Req, Env = #{app := cowboy, module := Module, extra_state := ExtraState}) ->
    try Module:init(Req, ExtraState) of
        {ok, Req2, _State} ->
            Result = terminate(normal, Req2, Module),
            {ok, Req2, Env#{result => Result}};
        {Mod, Req2, State} ->
            Mod:upgrade(Req2, Env, Module, State);
        {Mod, Req2, State, Opts} ->
            Mod:upgrade(Req2, Env, Module, State, Opts)
    catch Class:Reason:Stacktrace ->
            PrettyST = pretty_st(Stacktrace),
            FormattedST = [ bstring:join(X, <<"">>) || X <- PrettyST ],
            FormattedMsg = io_lib:format("~s: ~p", [Class, Reason]),
            terminate({crash, Class, Reason}, Req, Module),
            case nova_router:lookup_url(500) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, State0} = nova_basic_handler:handle_ok({ok, #{status => "Error 500",
                                                                       message => FormattedMsg,
                                                                       stacktrace => FormattedST},
                                                                 #{view => nova_error}}, {dummy, dummy}, ExtraState),
                    render_response(State0);
                {ok, #mmf{module = EMod, function = EFunc}, _PathState} ->
                    %% Show this view - how?
                    {ok, State0} = nova_basic_handler:handle_ok({ok, #{status => "Error 500",
                                                                       message => FormattedMsg,
                                                                       stacktrace => FormattedST}}, {EMod, EFunc}, ExtraState),
                    render_response(State0)
            end,
            erlang:raise(Class, Reason, Stacktrace)
    end;
execute(Req, Env = #{app := _App, module := Module, function := Function, controller_data := CtrlData}) ->
    State = CtrlData#{req => Req},
    try Module:Function(State) of
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
            PrettyST = pretty_st(Stacktrace),
            FormattedST = [ bstring:join(X, <<"">>) || X <- PrettyST ],
            FormattedMsg = io_lib:format("~s: ~p", [Class, Reason]),
            terminate(Reason, Req, Module),
            case nova_router:lookup_url(500) of
                {error, _} ->
                    %% Render the internal view of nova
                    {ok, State0} = nova_basic_handler:handle_ok({ok, #{status => "Error 500",
                                                                       message => FormattedMsg,
                                                                       stacktrace => FormattedST},
                                                                 #{view => nova_error}}, {dummy, dummy}, State),
                    render_response(State0);
                {ok, #mmf{module = EMod, function = EFunc}, _PathState} ->
                    %% Show this view - how?
                    {ok, State0} = nova_basic_handler:handle_ok({ok, #{status => "Error 500",
                                                                       message => FormattedMsg,
                                                                       stacktrace => FormattedST}}, {EMod, EFunc}, State),
                    render_response(State0)
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


pretty_st([]) -> [];
pretty_st([{Mod, Func, Arity, ExtraArgs}|Stacktrace]) when is_integer(Arity) ->
    Output = [
              atom_to_binary(Mod, utf8),
              <<":">>,
              atom_to_binary(Func, utf8),
              <<"/">>,
              integer_to_binary(Arity)
             ],
    Output0 =
        case proplists:get_value(file, ExtraArgs) of
            undefined ->
                Output;
            File ->
                %% Then line is also present
                Line = proplists:get_value(line, ExtraArgs),

                Output ++ [
                           <<" (">>,
                           list_to_binary(File),
                           <<":">>,
                           integer_to_binary(Line),
                           <<")">>
                          ]
        end,
    [Output0|pretty_st(Stacktrace)];
pretty_st([{Mod, Func, Args, ExtraArgs}|Stacktrace]) when is_list(Args) ->
    ArgsStrList = [ to_binary(X) || X <- Args ],
    ArgsStr = bstring:join(ArgsStrList, <<", ">>),
    Output = [
              atom_to_binary(Mod, utf8),
              <<":">>,
              atom_to_binary(Func, utf8),
              <<"(">>,
              ArgsStr,
              <<" )">>
             ],
    Output0 =
        case proplists:get_value(file, ExtraArgs) of
            undefined ->
                Output;
            File ->
                %% Then line is also present
                Line = proplists:get_value(line, ExtraArgs),

                Output ++ [
                           <<" (">>,
                           list_to_binary(File),
                           <<":">>,
                           integer_to_binary(Line),
                           <<")">>
                          ]
        end,
    [Output0|pretty_st(Stacktrace)].


to_binary(Int) when is_integer(Int)   -> integer_to_binary(Int);
to_binary(Float) when is_float(Float) -> float_to_binary(Float);
to_binary(Str) when is_list(Str)      -> list_to_binary(Str);
to_binary(Bin) when is_binary(Bin)    -> Bin;
to_binary(T)                          -> list_to_binary(io_lib:format("~tp", [T])).
