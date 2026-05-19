%% @doc Plugin that redirects HTTP requests to HTTPS.
%%
%% Should be used with nova_secure_headers_plugin's HSTS option for
%% complete TLS enforcement.
%%
%% == Options ==
%% <ul>
%%   <li>`host' — override redirect host (default: use request host)</li>
%%   <li>`port' — override redirect port (omitted if 443)</li>
%%   <li>`excluded_paths' — list of path prefixes to skip (e.g. health checks)</li>
%% </ul>
-module(nova_force_ssl_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% Pre-request: redirect HTTP to HTTPS
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req = #{scheme := <<"https">>}, _Env, _Options, State) ->
    {ok, Req, State};
pre_request(Req = #{scheme := Scheme, path := Path}, _Env, Options, State) when Scheme =/= <<"https">> ->
    ExcludedPaths = maps:get(excluded_paths, Options, []),
    case is_excluded(Path, ExcludedPaths) of
        true ->
            {ok, Req, State};
        false ->
            Location = build_https_url(Req, Options),
            Reply = cowboy_req:reply(301, #{<<"location">> => Location}, Req),
            {stop, Reply, State}
    end;
pre_request(Req, _Env, _Options, State) ->
    %% No scheme info (e.g. behind TLS offload without x-forwarded-proto)
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% Post-request: pass-through
%%--------------------------------------------------------------------
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% Plugin info
%%--------------------------------------------------------------------
plugin_info() ->
    #{title => <<"Nova Force SSL Plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Redirects HTTP requests to HTTPS.">>,
      options => [
          {host, <<"Override redirect host (default: request host)">>},
          {port, <<"Override redirect port (omitted if 443)">>},
          {excluded_paths, <<"Path prefixes to skip (e.g. health checks)">>}
      ]}.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

build_https_url(#{host := Host, path := Path, qs := Qs}, Options) ->
    RedirectHost = maps:get(host, Options, Host),
    Port = maps:get(port, Options, 443),
    PortStr = case Port of
        443 -> <<>>;
        P -> iolist_to_binary([<<":" >>, integer_to_binary(P)])
    end,
    QsStr = case Qs of
        <<>> -> <<>>;
        _ -> <<"?", Qs/binary>>
    end,
    <<"https://", RedirectHost/binary, PortStr/binary, Path/binary, QsStr/binary>>.

is_excluded(_Path, []) ->
    false;
is_excluded(Path, [Prefix | Rest]) ->
    case binary:match(Path, Prefix) of
        {0, _} -> true;
        _ -> is_excluded(Path, Rest)
    end.
