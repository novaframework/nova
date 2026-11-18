-module(nova_listener_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CAPABLE, #{protocols => [http1, http2, http3], cleartext => [http1, http2], websocket => [http1]}).

configured_test_() ->
    {foreach,
     fun() ->
             application:set_env(nova, bootstrap_application, my_app)
     end,
     fun(_) ->
             application:unset_env(nova, bootstrap_application),
             application:unset_env(nova, listeners),
             application:unset_env(nova, cowboy_configuration)
     end,
     [
      fun without_configuration_there_is_one_listener/0,
      fun the_only_listener_serves_the_bootstrap_application/0,
      fun one_of_several_listeners_names_its_applications/0,
      fun listeners_come_in_the_order_of_their_names/0,
      fun tls_changes_the_defaults/0,
      fun a_version_can_carry_options/0,
      fun two_listeners_on_one_port_are_refused/0,
      fun the_same_port_on_two_addresses_is_two_listeners/0,
      fun a_setting_nova_does_not_know_is_refused/0,
      fun a_wrong_setting_is_named/0,
      fun listeners_that_are_not_a_map_are_refused/0,
      fun cowboy_configuration_is_one_listener/0,
      fun cowboy_configuration_with_ssl/0,
      fun listeners_win_over_cowboy_configuration/0
     ]}.

without_configuration_there_is_one_listener() ->
    ?assertEqual({ok, [#{name => default,
                         ip => {0, 0, 0, 0},
                         port => 8080,
                         tls => false,
                         protocols => [{http1, #{}}, {http2, #{}}],
                         applications => [my_app],
                         steps => nova_pipeline:steps(),
                         server_opts => #{compress => true}}]},
                 nova_listener:configured()).

the_only_listener_serves_the_bootstrap_application() ->
    application:set_env(nova, listeners, #{web => #{port => 4000}}),
    ?assertMatch({ok, [#{name := web, port := 4000, applications := [my_app], protocols := [{http1, #{}}],
                         server_opts := #{}}]},
                 nova_listener:configured()).

one_of_several_listeners_names_its_applications() ->
    application:set_env(nova, listeners, #{web => #{port => 4000, applications => [my_app]},
                                           admin => #{port => 4001}}),
    ?assertEqual({error, {invalid_listener_setting, admin, applications, undefined}}, nova_listener:configured()).

listeners_come_in_the_order_of_their_names() ->
    application:set_env(nova, listeners, #{web => #{port => 4000, applications => [my_app]},
                                           admin => #{port => 4001, applications => [my_admin]}}),
    {ok, Listeners} = nova_listener:configured(),
    ?assertEqual([admin, web], [Name || #{name := Name} <- Listeners]).

tls_changes_the_defaults() ->
    application:set_env(nova, listeners, #{web => #{tls => #{certfile => "cert.pem", keyfile => "key.pem"}}}),
    {ok, [#{port := Port, tls := [_ | _] = Tls, protocols := Protocols}]} = nova_listener:configured(),
    ?assertEqual(8443, Port),
    ?assertEqual([{certfile, "cert.pem"}, {keyfile, "key.pem"}], lists:sort(Tls)),
    ?assertEqual([http1, http2], nova_listener:protocol_names(Protocols)).

a_version_can_carry_options() ->
    application:set_env(nova, listeners,
                        #{web => #{protocols => [http1, {http2, #{max_concurrent_streams => 10}}]}}),
    ?assertMatch({ok, [#{protocols := [{http1, #{}}, {http2, #{max_concurrent_streams := 10}}]}]},
                 nova_listener:configured()).

two_listeners_on_one_port_are_refused() ->
    application:set_env(nova, listeners, #{web => #{port => 4000, applications => [my_app]},
                                           admin => #{port => 4000, applications => [my_admin]}}),
    ?assertEqual({error, {listeners_on_one_port, {0, 0, 0, 0}, 4000}}, nova_listener:configured()).

the_same_port_on_two_addresses_is_two_listeners() ->
    application:set_env(nova, listeners,
                        #{web => #{ip => {10, 0, 0, 1}, port => 4000, applications => [my_app]},
                          admin => #{ip => {127, 0, 0, 1}, port => 4000, applications => [my_admin]}}),
    ?assertMatch({ok, [_, _]}, nova_listener:configured()).

a_setting_nova_does_not_know_is_refused() ->
    application:set_env(nova, listeners, #{web => #{prot => 4000, ssl => true}}),
    ?assertEqual({error, {unknown_listener_settings, web, [prot, ssl]}}, nova_listener:configured()).

a_wrong_setting_is_named() ->
    [begin
         application:set_env(nova, listeners, #{web => #{Setting => Value}}),
         ?assertEqual({error, {invalid_listener_setting, web, Setting, Value}}, nova_listener:configured())
     end || {Setting, Value} <- [{port, "4000"},
                                 {port, 70000},
                                 {ip, ~"127.0.0.1"},
                                 {tls, true},
                                 {protocols, []},
                                 {protocols, http2},
                                 {protocols, [http1, http1]},
                                 {protocols, [http1, {http2, []}]},
                                 {protocols, [http1, spdy]},
                                 {applications, []},
                                 {applications, my_app},
                                 {steps, []},
                                 {server_opts, []}]],
    application:set_env(nova, listeners, #{~"web" => #{}}),
    ?assertEqual({error, {invalid_listener_name, ~"web"}}, nova_listener:configured()),
    application:set_env(nova, listeners, #{web => [{port, 4000}]}),
    ?assertEqual({error, {invalid_listener, web, [{port, 4000}]}}, nova_listener:configured()).

listeners_that_are_not_a_map_are_refused() ->
    application:set_env(nova, listeners, [{web, #{}}]),
    ?assertEqual({error, {invalid_listeners, [{web, #{}}]}}, nova_listener:configured()),
    application:set_env(nova, listeners, #{}),
    ?assertEqual({error, {invalid_listeners, #{}}}, nova_listener:configured()).

cowboy_configuration_is_one_listener() ->
    application:set_env(nova, cowboy_configuration, #{port => 4000,
                                                      ip => {127, 0, 0, 1},
                                                      options => #{idle_timeout => 1000},
                                                      stream_handlers => [my_stream_h],
                                                      middleware_handlers => [my_step]}),
    ?assertEqual({ok, [#{name => default,
                         ip => {127, 0, 0, 1},
                         port => 4000,
                         tls => false,
                         protocols => [{http1, #{}}, {http2, #{}}],
                         applications => [my_app],
                         steps => [my_step],
                         server_opts => #{idle_timeout => 1000, stream_handlers => [my_stream_h]}}]},
                 nova_listener:configured()).

cowboy_configuration_with_ssl() ->
    application:set_env(nova, cowboy_configuration, #{use_ssl => true,
                                                      port => 4000,
                                                      ssl_port => 4443,
                                                      ssl_options => #{certfile => "cert.pem"}}),
    ?assertMatch({ok, [#{port := 4443, tls := [{certfile, "cert.pem"}]}]}, nova_listener:configured()),
    application:set_env(nova, cowboy_configuration, #{use_ssl => true, ca_cert => "ca.pem", cert => "cert.pem"}),
    ?assertMatch({ok, [#{port := 8443, tls := [{certfile, "cert.pem"}, {cacertfile, "ca.pem"}]}]},
                 nova_listener:configured()).

listeners_win_over_cowboy_configuration() ->
    application:set_env(nova, cowboy_configuration, #{port => 4000}),
    application:set_env(nova, listeners, #{web => #{port => 5000}}),
    ?assertMatch({ok, [#{name := web, port := 5000}]}, nova_listener:configured()).

a_server_serves_what_it_can_test() ->
    {ok, Clear} = nova_listener:normalize(web, #{protocols => [http1, http2]}, #{applications => [my_app]}),
    {ok, Tls} = nova_listener:normalize(web, #{tls => #{certfile => "cert.pem"}, protocols => [http1, http2, http3]},
                                        #{applications => [my_app]}),
    {ok, ClearH3} = nova_listener:normalize(web, #{protocols => [http1, http3]}, #{applications => [my_app]}),
    ?assertEqual(ok, nova_listener:check(Clear, my_server, ?CAPABLE)),
    ?assertEqual(ok, nova_listener:check(Tls, my_server, ?CAPABLE)),
    ?assertEqual({error, {listener_protocols_need_tls, web, [http3]}},
                 nova_listener:check(ClearH3, my_server, ?CAPABLE)),
    ?assertEqual({error, {listener_protocols_not_supported, web, [http3], my_server}},
                 nova_listener:check(Tls, my_server, maps:put(protocols, [http1, http2], ?CAPABLE))),
    ?assertEqual({error, {listener_protocols_need_tls, web, [http2]}},
                 nova_listener:check(Clear, my_server, maps:put(cleartext, [http1], ?CAPABLE))).
