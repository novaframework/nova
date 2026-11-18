-module(nova_listener).
-moduledoc """
The listeners of a node: where Nova accepts connections, and which HTTP
versions each of them speaks.

```erlang
{nova, [
    {bootstrap_application, my_app},
    {listeners, #{
        public => #{port => 8443,
                    tls => #{certfile => "cert.pem", keyfile => "key.pem"},
                    protocols => [http1, http2, http3],
                    applications => [my_app]},
        admin => #{ip => {127, 0, 0, 1},
                   port => 9090,
                   protocols => [http1],
                   applications => [my_admin]}
    }}
]}
```

A listener has a name, which is the key it is configured under, and these
settings:

- `port`: `8080`, or `8443` with `tls`, when it is not set
- `ip`: the address to bind, every address when it is not set
- `tls`: the options of the `ssl` application for a server, as a map or a
  list. Without it the listener is not encrypted.
- `protocols`: the HTTP versions the listener serves, out of `http1`,
  `http2` and `http3`. A version is written `{http2, Options}` to give it
  options of the server. When it is not set a listener with `tls` serves
  `[http1, http2]` and one without serves `[http1]`.
- `applications`: the Nova applications whose routes the listener serves.
  The only listener of a node serves the bootstrap application when this is
  not set.
- `steps`: the steps a request goes through, those of `nova_pipeline` when it
  is not set
- `server_opts`: options of the server package, as it documents them

HTTP/1.1 and HTTP/2 share the TCP port. HTTP/3 is served over UDP on the same
port number, and needs `tls`.

The node does not start when a listener asks for something the server
cannot do, such as `http3` on a server without it, or `http2` without `tls`
on a server that only speaks it encrypted.

A node with neither `listeners` nor `cowboy_configuration` has one listener
named `default` with these defaults, which serves the bootstrap application.
""".

-export([
         configured/0,
         from_legacy/1,
         runtime_settings/1,
         normalize/3,
         check/3,
         protocol_names/1
        ]).

-export_type([name/0, listener/0]).

-define(PORT, 8080).
-define(TLS_PORT, 8443).
-define(KEYS, [port, ip, tls, protocols, applications, steps, server_opts]).
-define(LEGACY_KEYS, [use_ssl, ssl_port, ssl_options, ca_cert, cert, options, stream_handlers,
                      middleware_handlers]).

-type name() :: atom().

-doc "A listener as a server gets it: checked, and with every setting filled in.".
-type listener() :: #{name := name(),
                      ip := inet:ip_address() | string(),
                      port := inet:port_number(),
                      tls := false | [ssl:tls_server_option()],
                      protocols := [{nova_server:protocol(), map()}],
                      applications := [atom()],
                      steps := [module()],
                      server_opts := map()}.

-doc "The listeners the environment of the `nova` application asks for.".
-spec configured() -> {ok, [listener()]} | {error, term()}.
configured() ->
    Bootstrap = application:get_env(nova, bootstrap_application, undefined),
    case application:get_env(nova, listeners) of
        {ok, Listeners} when is_map(Listeners), map_size(Listeners) > 0 ->
            Only = map_size(Listeners) =:= 1,
            normalize_all(lists:sort(maps:to_list(Listeners)), defaults(Bootstrap, Only), []);
        {ok, Other} ->
            {error, {invalid_listeners, Other}};
        undefined ->
            Settings = case application:get_env(nova, cowboy_configuration) of
                           {ok, Legacy} -> from_legacy(Legacy);
                           undefined -> #{}
                       end,
            normalize_all([{default, Settings}], defaults(Bootstrap, true), [])
    end.

%% The applications of a listener that names none: the bootstrap application,
%% when the listener is the only one.
defaults(undefined, _Only) -> #{};
defaults(Bootstrap, true) -> #{applications => [Bootstrap]};
defaults(_Bootstrap, false) -> #{}.

normalize_all([], _Defaults, Listeners) ->
    unique_binds(lists:reverse(Listeners));
normalize_all([{Name, Settings} | Rest], Defaults, Listeners) ->
    case normalize(Name, Settings, Defaults) of
        {ok, Listener} -> normalize_all(Rest, Defaults, [Listener | Listeners]);
        {error, Reason} -> {error, Reason}
    end.

unique_binds(Listeners) ->
    Binds = [{Ip, Port} || #{ip := Ip, port := Port} <- Listeners],
    case Binds -- lists:usort(Binds) of
        [] -> {ok, Listeners};
        [{Ip, Port} | _] -> {error, {listeners_on_one_port, Ip, Port}}
    end.

-doc false.
%% The settings of the `cowboy_configuration` key of Nova before 1.0 as those
%% of a listener. `use_ssl` picked `ssl_port` over `port`, and Cowboy served
%% HTTP/2 next to HTTP/1.1 with or without TLS.
-spec from_legacy(map()) -> map().
from_legacy(Legacy) ->
    Tls = case maps:get(use_ssl, Legacy, false) of
              false -> false;
              _ -> legacy_tls(Legacy)
          end,
    Port = case Tls of
               false -> maps:get(port, Legacy, ?PORT);
               _ -> maps:get(ssl_port, Legacy, ?TLS_PORT)
           end,
    ServerOpts0 = maps:get(options, Legacy, #{compress => true}),
    ServerOpts = case Legacy of
                     #{stream_handlers := StreamHandlers} -> ServerOpts0#{stream_handlers => StreamHandlers};
                     #{} -> ServerOpts0
                 end,
    Listener = #{port => Port,
                 tls => Tls,
                 protocols => [http1, http2],
                 server_opts => ServerOpts},
    (maps:merge(Listener, maps:with([ip], Legacy)))
        #{steps => maps:get(middleware_handlers, Legacy, nova_pipeline:steps())}.

-doc false.
%% What `nova_sup:add_application/2` is given: the settings of a listener, or
%% those Nova took before 1.0, which are told apart by the keys only they
%% have.
-spec runtime_settings(map()) -> map().
runtime_settings(Settings) ->
    case maps:size(maps:with(?LEGACY_KEYS, Settings)) of
        0 -> Settings;
        _ -> from_legacy(Settings)
    end.

legacy_tls(#{ca_cert := CACert, cert := Cert}) ->
    [{certfile, Cert}, {cacertfile, CACert}];
legacy_tls(Legacy) ->
    maps:get(ssl_options, Legacy, #{}).

-doc false.
-spec normalize(term(), term(), map()) -> {ok, listener()} | {error, term()}.
normalize(Name, _Settings, _Defaults) when not is_atom(Name) ->
    {error, {invalid_listener_name, Name}};
normalize(Name, Settings, Defaults) when is_map(Settings) ->
    case maps:keys(maps:without(?KEYS, Settings)) of
        [] ->
            normalize_settings(Name, maps:merge(Defaults, Settings));
        Unknown ->
            {error, {unknown_listener_settings, Name, lists:sort(Unknown)}}
    end;
normalize(Name, Settings, _Defaults) ->
    {error, {invalid_listener, Name, nova_redact:tls_options(Settings)}}.

normalize_settings(Name, Settings) ->
    try
        Tls = tls(maps:get(tls, Settings, false)),
        DefaultPort = case Tls of false -> ?PORT; _ -> ?TLS_PORT end,
        DefaultProtocols = case Tls of false -> [http1]; _ -> [http1, http2] end,
        {ok, #{name => Name,
               ip => ip(maps:get(ip, Settings, {0, 0, 0, 0})),
               port => port(maps:get(port, Settings, DefaultPort)),
               tls => Tls,
               protocols => protocols(maps:get(protocols, Settings, DefaultProtocols)),
               applications => applications(maps:get(applications, Settings, undefined)),
               steps => steps(maps:get(steps, Settings, nova_pipeline:steps())),
               server_opts => server_opts(maps:get(server_opts, Settings, #{}))}}
    catch
        throw:{invalid, tls, _Value} ->
            {error, {invalid_listener_setting, Name, tls, redacted}};
        throw:{invalid, Setting, Value} ->
            {error, {invalid_listener_setting, Name, Setting, Value}}
    end.

tls(false) -> false;
tls(Options) when is_map(Options) -> maps:to_list(Options);
tls(Options) when is_list(Options) -> Options;
tls(Other) -> throw({invalid, tls, Other}).

ip(Ip) when is_tuple(Ip), tuple_size(Ip) =:= 4; is_tuple(Ip), tuple_size(Ip) =:= 8 -> Ip;
ip(Host) when is_list(Host) -> Host;
ip(Other) -> throw({invalid, ip, Other}).

port(Port) when is_integer(Port), Port >= 0, Port =< 65535 -> Port;
port(Other) -> throw({invalid, port, Other}).

protocols(Protocols) when is_list(Protocols), Protocols =/= [] ->
    Normalized = [protocol(Protocol, Protocols) || Protocol <- Protocols],
    Names = protocol_names(Normalized),
    case length(Names) =:= length(lists:usort(Names)) of
        true -> Normalized;
        false -> throw({invalid, protocols, Protocols})
    end;
protocols(Other) ->
    throw({invalid, protocols, Other}).

protocol(Name, _Protocols) when Name =:= http1; Name =:= http2; Name =:= http3 ->
    {Name, #{}};
protocol({Name, Options}, _Protocols)
  when (Name =:= http1 orelse Name =:= http2 orelse Name =:= http3), is_map(Options) ->
    {Name, Options};
protocol(_Other, Protocols) ->
    throw({invalid, protocols, Protocols}).

applications(undefined) ->
    throw({invalid, applications, undefined});
applications(Applications) when is_list(Applications), Applications =/= [] ->
    case lists:all(fun is_atom/1, Applications) of
        true -> Applications;
        false -> throw({invalid, applications, Applications})
    end;
applications(Other) ->
    throw({invalid, applications, Other}).

steps(Steps) when is_list(Steps), Steps =/= [] ->
    case lists:all(fun is_atom/1, Steps) of
        true -> Steps;
        false -> throw({invalid, steps, Steps})
    end;
steps(Other) ->
    throw({invalid, steps, Other}).

server_opts(Options) when is_map(Options) -> Options;
server_opts(Other) -> throw({invalid, server_opts, Other}).

-spec protocol_names([{nova_server:protocol(), map()}]) -> [nova_server:protocol()].
protocol_names(Protocols) ->
    [Name || {Name, _Options} <- Protocols].

-doc false.
%% Whether a server can serve the listener: every HTTP version it asks for,
%% with or without TLS as it asks.
-spec check(listener(), module(), nova_server:capabilities()) -> ok | {error, term()}.
check(#{name := Name, tls := Tls, protocols := Protocols}, Server,
      #{protocols := Supported, cleartext := Cleartext}) ->
    Asked = protocol_names(Protocols),
    case Asked -- Supported of
        [] when Tls =:= false ->
            case Asked -- Cleartext of
                [] -> ok;
                NeedTls -> {error, {listener_protocols_need_tls, Name, NeedTls}}
            end;
        [] ->
            ok;
        Unsupported ->
            {error, {listener_protocols_not_supported, Name, Unsupported, Server}}
    end.
