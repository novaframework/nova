-module(nova_cors_plugin).
-moduledoc false.
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
-doc "Pre-request callback".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                         {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, #{allow_origins := Origins} = Options, State) ->
    ReqWithOptions = add_cors_headers(Req, Origins, Options),
    continue(ReqWithOptions, State).

%%--------------------------------------------------------------------
-doc "Post-request callback".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                               {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
-doc "nova_plugin callback. Returns information about the plugin.".
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{
      title => <<"nova_cors_plugin">>,
      version => ~"0.3.0",
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Add CORS headers to request">>,
      options => [
                  {allow_origins, ~"<<\"*\">>, an origin, or a list of origins allowed to make cross-origin requests"},
                  {allow_headers, ~"Request headers a preflight allows. Default: the ones the preflight asks for"},
                  {allow_methods, ~"Methods a preflight allows. Default: <<\"*\">>"},
                  {max_age, ~"Seconds a browser may cache a preflight answer"}
                 ]
     }.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

continue(Req, State) ->
    case is_preflight(Req) of
        true -> {stop, cowboy_req:reply(204, Req), State};
        false -> {ok, Req, State}
    end.

%% Any other OPTIONS request is the application's to answer.
is_preflight(#{method := ~"OPTIONS"} = Req) ->
    cowboy_req:header(~"origin", Req) =/= undefined andalso
        cowboy_req:header(~"access-control-request-method", Req) =/= undefined;
is_preflight(_Req) ->
    false.

add_cors_headers(Req, ~"*", Options) ->
    preflight_headers(cowboy_req:set_resp_header(~"access-control-allow-origin", ~"*", Req), Options);
add_cors_headers(Req, Origins, Options) ->
    %% The answer depends on the Origin header, so caches must key on it.
    Req0 = vary_on_origin(Req),
    case cowboy_req:header(~"origin", Req) of
        undefined ->
            Req0;
        Origin ->
            case lists:member(Origin, values(Origins)) of
                true ->
                    Req1 = cowboy_req:set_resp_header(~"access-control-allow-origin", Origin, Req0),
                    preflight_headers(Req1, Options);
                false ->
                    Req0
            end
    end.

vary_on_origin(Req) ->
    case cowboy_req:resp_header(~"vary", Req, undefined) of
        Vary when is_binary(Vary) ->
            Listed = [string:lowercase(string:trim(V)) || V <- binary:split(Vary, ~",", [global])],
            case lists:member(~"origin", Listed) orelse lists:member(~"*", Listed) of
                true -> Req;
                false -> cowboy_req:set_resp_header(~"vary", <<Vary/binary, ", origin">>, Req)
            end;
        _ ->
            cowboy_req:set_resp_header(~"vary", ~"origin", Req)
    end.

-spec values(binary() | [unicode:chardata()]) -> [binary()].
values(Value) when is_binary(Value) ->
    [Value];
values(Values) when is_list(Values) ->
    case io_lib:printable_unicode_list(Values) of
        true -> [text(Values)];
        false -> [text(Value) || Value <- Values]
    end.

-spec text(unicode:chardata()) -> binary().
text(Value) when is_binary(Value) ->
    Value;
text(Value) ->
    case unicode:characters_to_binary(Value) of
        Text when is_binary(Text) -> Text
    end.

%% "*" in Access-Control-Allow-Headers never covers Authorization, so the
%% default is to allow what the preflight asks for. That is only safe because
%% access-control-allow-origin is sent for allowed origins alone.
preflight_headers(Req, Options) ->
    Headers = case maps:get(allow_headers, Options, undefined) of
                  undefined -> requested_headers(Req);
                  Configured -> join(Configured)
              end,
    Req0 = cowboy_req:set_resp_header(~"access-control-allow-headers", Headers, Req),
    Req1 = cowboy_req:set_resp_header(~"access-control-allow-methods",
                                      join(maps:get(allow_methods, Options, ~"*")), Req0),
    case maps:get(max_age, Options, undefined) of
        MaxAge when is_integer(MaxAge) ->
            cowboy_req:set_resp_header(~"access-control-max-age", integer_to_binary(MaxAge), Req1);
        _ ->
            Req1
    end.

requested_headers(Req) ->
    case cowboy_req:header(~"access-control-request-headers", Req) of
        undefined -> ~"*";
        Requested -> Requested
    end.

join(Value) ->
    case values(Value) of
        [] -> <<>>;
        [First | Rest] -> iolist_to_binary([First | [[~", ", Next] || Next <- Rest]])
    end.
