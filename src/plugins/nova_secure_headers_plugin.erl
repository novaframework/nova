-module(nova_secure_headers_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-define(DEFAULT_HEADERS, #{
    <<"x-frame-options">> => <<"DENY">>,
    <<"x-content-type-options">> => <<"nosniff">>,
    <<"x-xss-protection">> => <<"1; mode=block">>,
    <<"referrer-policy">> => <<"strict-origin-when-cross-origin">>,
    <<"permissions-policy">> => <<"geolocation=(), camera=(), microphone=()">>
}).

%%--------------------------------------------------------------------
%% Pre-request: set security headers on every response
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    Defaults = ?DEFAULT_HEADERS,
    Extra = maps:get(extra_headers, Options, #{}),
    Overrides = maps:get(headers, Options, #{}),
    Merged = maps:merge(maps:merge(Defaults, Extra), Overrides),
    Req1 = add_hsts(Req, Options),
    Req2 = add_csp(Req1, Options),
    Req3 = maps:fold(fun(Name, Value, ReqAcc) ->
        cowboy_req:set_resp_header(Name, Value, ReqAcc)
    end, Req2, Merged),
    {ok, Req3, State}.

%%--------------------------------------------------------------------
%% Post-request: pass-through
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% Plugin info
%%--------------------------------------------------------------------
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => <<"Nova Secure Headers Plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Sets security-related HTTP response headers.">>,
      options => [
          {headers, <<"Map of header name => value to override defaults">>},
          {extra_headers, <<"Map of additional headers to merge with defaults">>},
          {hsts, <<"true | false — enable Strict-Transport-Security (default: false)">>},
          {hsts_max_age, <<"Max-age in seconds for HSTS (default: 31536000)">>},
          {hsts_include_subdomains, <<"Include subdomains in HSTS (default: true)">>},
          {csp, <<"Content-Security-Policy value (binary), or false to disable">>}
      ]}.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

add_hsts(Req, #{hsts := true} = Options) ->
    MaxAge = maps:get(hsts_max_age, Options, 31536000),
    IncludeSub = maps:get(hsts_include_subdomains, Options, true),
    Value = case IncludeSub of
        true ->
            iolist_to_binary([<<"max-age=">>, integer_to_binary(MaxAge),
                              <<"; includeSubDomains">>]);
        false ->
            iolist_to_binary([<<"max-age=">>, integer_to_binary(MaxAge)])
    end,
    cowboy_req:set_resp_header(<<"strict-transport-security">>, Value, Req);
add_hsts(Req, _Options) ->
    Req.

add_csp(Req, #{csp := false}) ->
    Req;
add_csp(Req, #{csp := Policy}) when is_binary(Policy) ->
    cowboy_req:set_resp_header(<<"content-security-policy">>, Policy, Req);
add_csp(Req, _Options) ->
    Req.
