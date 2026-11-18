-module(nova_applications_tests).

-include_lib("eunit/include/eunit.hrl").

%% A crash of the process is reported with its state and the request it
%% had, and the settings of a listener are in both. The events of a
%% debugged process have every request in them.
status_test() ->
    Tls = [{certfile, "cert.pem"}, {key, {'RSAPrivateKey', ~"hunter2"}}, {password, "hunter2"}],
    Request = {'$gen_call', {self(), make_ref()}, {add, my_app, #{port => 8443, tls => Tls}}},
    Bind = {{0, 0, 0, 0}, 8443},
    State = {state, [{added, my_app, #{tls => Tls}, my_app, Bind, true}], [{started, my_app, #{tls => Tls}, Bind}],
             undefined},
    Status = nova_applications:format_status(#{state => State,
                                               message => Request,
                                               reason => shutdown,
                                               log => [{in, Request}]}),
    Safe = [{certfile, "cert.pem"}, {key, redacted}, {password, redacted}],
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~0p", [Status])), ~"hunter2")),
    ?assertEqual(#{state => {state, [{added, my_app, #{tls => Safe}, my_app, Bind, true}],
                             [{started, my_app, #{tls => Safe}, Bind}], undefined},
                   message => {'$gen_call', element(2, Request), {add, my_app, #{port => 8443, tls => Safe}}},
                   reason => shutdown,
                   log => []},
                 Status),
    ?assertEqual(#{reason => shutdown}, nova_applications:format_status(#{reason => shutdown})).

not_started_test() ->
    ?assertEqual(undefined, whereis(nova_applications)),
    ?assertEqual({error, server_not_started}, nova_applications:add(my_app, #{port => 8080})),
    ?assertEqual({error, not_found}, nova_applications:remove(my_app)),
    ?assertEqual({error, server_not_started}, nova_applications:listening(self())),
    ?assertEqual(ok, nova_applications:draining()).
