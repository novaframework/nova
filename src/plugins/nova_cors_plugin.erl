-module(nova_cors_plugin).
-moduledoc """
Sets the CORS response headers and answers preflight requests. Use it as a
`pre_request` plugin.

`allow_origins` is required: `~"*"`, one origin or a list of origins. With
`~"*"` every response gets `access-control-allow-origin: *`. With origins,
the response gets `vary: origin`, and `access-control-allow-origin` with the
origin of the request when that origin is listed. A request from another
origin gets no CORS headers.

Together with `access-control-allow-origin` the response gets
`access-control-allow-headers` (`allow_headers`, by default what the
preflight asks for), `access-control-allow-methods` (`allow_methods`, by
default `*`) and, when `max_age` is an integer, `access-control-max-age`.

A preflight, an `OPTIONS` request with the `origin` and
`access-control-request-method` headers, is answered with a `204` and goes
no further. Any other `OPTIONS` request goes on to the application.
""".
-behaviour(nova_plugin).

-export([
         pre_security/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
-doc """
Sets the CORS headers on the response. Returns `{stop, Req, State}` after
answering a preflight with a `204`, and `{ok, Req, State}` for every other
request. `Options` without `allow_origins` matches no clause.

Runs ahead of the security callback: a preflight carries no credentials, and
a browser only shows a refusal to the page when it has the headers.
""".
-spec pre_security(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                          {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_security(Req, _Env, #{allow_origins := Origins} = Options, State) ->
    ReqWithOptions = add_cors_headers(Req, Origins, Options),
    continue(ReqWithOptions, State).

%%--------------------------------------------------------------------
-doc "Does nothing: the plugin works in `pre_security/4`.".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                               {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
-doc "The title, version, description and options of the plugin.".
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

is_preflight(#{method := ~"OPTIONS"} = Req) ->
    cowboy_req:header(~"origin", Req) =/= undefined andalso
        cowboy_req:header(~"access-control-request-method", Req) =/= undefined;
is_preflight(_Req) ->
    false.

add_cors_headers(Req, ~"*", Options) ->
    preflight_headers(cowboy_req:set_resp_header(~"access-control-allow-origin", ~"*", Req), Options);
add_cors_headers(Req, Origins, Options) ->
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
