-module(nova_applications_tests).

-include_lib("eunit/include/eunit.hrl").

%% A crash of the process is reported with its state and the request it
%% had, and the settings of a listener are in both.
status_test() ->
    Tls = [{certfile, "cert.pem"}, {key, {'RSAPrivateKey', ~"hunter2"}}, {password, "hunter2"}],
    Request = {'$gen_call', {self(), make_ref()}, {add, my_app, #{port => 8443, tls => Tls}}},
    {ok, State0} = nova_applications:init([]),
    Status = nova_applications:format_status(#{state => {state, [{added, my_app, #{tls => Tls}, {{0, 0, 0, 0}, 8443},
                                                                  false, true}],
                                                         element(3, State0)},
                                               message => Request,
                                               reason => shutdown,
                                               log => []}),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~0p", [Status])), ~"hunter2")),
    ?assertMatch(#{state := {state, [{added, my_app, #{tls := [{certfile, "cert.pem"}, {key, redacted},
                                                              {password, redacted}]}, _, false, true}], undefined},
                   message := {'$gen_call', _, {add, my_app, #{port := 8443}}},
                   reason := shutdown},
                 Status).

not_started_test() ->
    ?assertEqual(undefined, whereis(nova_applications)),
    ?assertEqual({error, server_not_started}, nova_applications:add(my_app, #{port => 8080})),
    ?assertEqual({error, not_found}, nova_applications:remove(my_app)),
    ?assertEqual(ok, nova_applications:draining()).
