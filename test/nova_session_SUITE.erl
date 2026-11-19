%%% Session cookies over a real Cowboy listener, against nova_test_app.
-module(nova_session_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

%% Logger handler callback: forwards every event to the test process.
-export([log/2]).

-export([
         lazy_cookie/1,
         attributes/1,
         host_prefix/1,
         old_name/1,
         survives/1,
         reason_phrase/1,
         delete/1,
         bad_name_runtime/1,
         bad_name_boot/1,
         delete_no_stream_h/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     lazy_cookie,
     attributes,
     host_prefix,
     old_name,
     survives,
     reason_phrase,
     delete,
     bad_name_runtime,
     bad_name_boot,
     {group, without_stream_h}
    ].

groups() ->
    [{without_stream_h, [], [delete_no_stream_h]}].

init_per_suite(Config) ->
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    persistent_term:put(nova_test_app_plugin, counters:new(2, [write_concurrency])),
    {ok, _} = application:ensure_all_started(inets),
    start(#{}, Config).

%% nova_session expires the cookie itself when the listener has no
%% nova_stream_h.
init_per_group(without_stream_h, Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    start(#{stream_handlers => [cowboy_compress_h, cowboy_stream_h]}, proplists:delete(base, Config)).

end_per_group(_Group, _Config) ->
    ok.

start(Configuration, Config) ->
    Port = nova_test_listener:free_port(),
    application:set_env(nova, cowboy_configuration, Configuration#{port => Port}),
    {ok, _} = application:ensure_all_started(nova_test_app),
    [{base, "http://localhost:" ++ integer_to_list(Port)} | Config].

end_per_suite(_Config) ->
    persistent_term:erase(nova_test_app_plugin),
    application:stop(nova_test_app),
    application:stop(nova),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% A request that does not write to the session gets no cookie.
lazy_cookie(Config) ->
    {200, Headers, _} = request(Config, "/session/untouched", undefined),
    ?assertEqual([], set_cookies(Headers)),
    {200, Read, _} = request(Config, "/session/get", undefined),
    ?assertEqual([], set_cookies(Read)),
    {200, Written, _} = request(Config, "/session/set/a", undefined),
    ?assertMatch([_], session_cookies(Written)).

%% Secure unless the environment is dev, and this suite runs in test. No
%% Max-Age: the cookie ends with the browser session.
attributes(Config) ->
    {200, Headers, _} = request(Config, "/session/set/a", undefined),
    [Cookie] = session_cookies(Headers),
    Attributes = attributes_of(Cookie),
    ?assert(lists:member("Path=/", Attributes)),
    ?assert(lists:member("HttpOnly", Attributes)),
    ?assert(lists:member("SameSite=Lax", Attributes)),
    ?assert(lists:member("Secure", Attributes)),
    ?assertEqual([], [A || "Max-Age" ++ _ = A <- Attributes]).

%% The listener of this suite has no TLS, as one behind a proxy that
%% terminates it has none: the cookie is the one a browser keeps over https.
host_prefix(Config) ->
    {200, Headers, _} = request(Config, "/session/set/a", undefined),
    [Cookie] = set_cookies(Headers),
    ?assertMatch("__Host-session_id=" ++ _, Cookie),
    Attributes = attributes_of(Cookie),
    ?assert(lists:member("Secure", Attributes)),
    ?assert(lists:member("Path=/", Attributes)),
    ?assertEqual([], [A || "Domain" ++ _ = A <- Attributes]).

%% A client that still has the cookie of a Nova that named it session_id has
%% no session: the id under the old name is not read, also when it is one a
%% session is stored under.
old_name(Config) ->
    {200, H1, _} = request(Config, "/session/set/kept", undefined),
    Id = session_id(H1),
    Old = [{"cookie", "session_id=" ++ Id}],
    {200, _, Body} = request_with(Config, "/session/get", Old),
    ?assertEqual(#{~"value" => null}, json:decode(Body)),
    {200, H2, _} = request_with(Config, "/session/set/again", Old),
    ?assertNotEqual(Id, session_id(H2)),
    {200, _, Kept} = request(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => ~"kept"}, json:decode(Kept)).

survives(Config) ->
    {200, H1, _} = request(Config, "/session/set/kept", undefined),
    Id = session_id(H1),
    {200, H2, Body} = request(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => ~"kept"}, json:decode(Body)),
    ?assertEqual([], session_cookies(H2)),
    {200, H3, _} = request(Config, "/session/set/again", Id),
    ?assertEqual([], session_cookies(H3)).

%% Cowboy takes a status as an integer or as a binary with the reason phrase.
reason_phrase(Config) ->
    [begin
         {200, Headers, ~"stored"} = request(Config, Path, undefined),
         {200, _, Body} = request(Config, "/session/get", session_id(Headers)),
         ?assertEqual(#{~"value" => ~"kept"}, json:decode(Body))
     end || Path <- ["/session/set-with-status-text/kept", "/session/set-and-stream/kept"]].

delete(Config) ->
    {200, H1, _} = request(Config, "/session/set/bye", undefined),
    Id = session_id(H1),
    {200, H2, _} = request(Config, "/session/delete", Id),
    [Cookie] = session_cookies(H2),
    ?assertMatch("__Host-session_id=;" ++ _, Cookie),
    ?assertNotEqual(nomatch, string:find(Cookie, "Max-Age=0")),
    {200, _, Body} = request(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => null}, json:decode(Body)).

%% cow_cookie raises on a name that is not a token. Raising in nova_stream_h
%% made Cowboy send its 500 back through the same handler.
bad_name_runtime(Config) ->
    nova_once:forget({nova_stream_h, invalid_cookie_logged, ~"app:sid"}),
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
    application:set_env(nova, session_cookie_name, ~"app:sid"),
    try
        [begin
             {200, Headers, _} = request_with(Config, "/session/set/a", [{"authorization", "Bearer AUTH_SECRET"}]),
             ?assertEqual([], set_cookies(Headers))
         end || _ <- [first, second]],
        timer:sleep(200),
        Logs = logged(),
        ?assertMatch([#{cookie := ~"app:sid"}],
                     [R || {report, #{msg := <<"Session cookie not set", _/binary>>} = R} <- Logs]),
        ?assertEqual(nomatch, string:find(io_lib:format("~p", [Logs]), "AUTH_SECRET"))
    after
        application:unset_env(nova, session_cookie_name),
        logger:remove_handler(?MODULE),
        nova_once:forget({nova_stream_h, invalid_cookie_logged, ~"app:sid"})
    end.

%% The same name stops Nova from starting, unless sessions are off. Leaves
%% Nova stopped; the next group starts it again.
bad_name_boot(_Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    application:set_env(nova, session_cookie_name, ~"app:sid"),
    try
        {error, Reason} = application:ensure_all_started(nova_test_app),
        ?assertNotEqual(nomatch, string:find(io_lib:format("~p", [Reason]), "invalid_session_cookie_config")),
        application:set_env(nova, use_sessions, false),
        {ok, _} = application:ensure_all_started(nova_test_app)
    after
        application:unset_env(nova, session_cookie_name),
        application:unset_env(nova, use_sessions),
        application:stop(nova_test_app),
        application:stop(nova)
    end.

%% Without nova_stream_h nothing gives a request a session id, so the client
%% brings one.
delete_no_stream_h(Config) ->
    Id = "brought-by-the-client",
    {200, H1, _} = request(Config, "/session/set/kept", Id),
    ?assertEqual([], session_cookies(H1)),
    {200, _, Kept} = request(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => ~"kept"}, json:decode(Kept)),
    {200, H2, _} = request(Config, "/session/delete", Id),
    [Cookie] = session_cookies(H2),
    ?assertMatch("__Host-session_id=;" ++ _, Cookie),
    ?assertNotEqual(nomatch, string:find(Cookie, "Max-Age=0")),
    {200, _, Body} = request(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => null}, json:decode(Body)).

%%====================================================================
%% Helpers
%%====================================================================

request(Config, Path, undefined) ->
    request_with(Config, Path, []);
request(Config, Path, SessionId) ->
    request_with(Config, Path, [{"cookie", "__Host-session_id=" ++ SessionId}]).

request_with(Config, Path, Headers) ->
    {ok, {{_, Status, _}, RespHeaders, Body}} =
        httpc:request(get, {?config(base, Config) ++ Path, Headers},
                      [{autoredirect, false}, {timeout, 5000}], [{body_format, binary}]),
    {Status, RespHeaders, Body}.

set_cookies(Headers) ->
    [Value || {Name, Value} <- Headers, string:lowercase(Name) =:= "set-cookie"].

session_cookies(Headers) ->
    [Value || "__Host-session_id=" ++ _ = Value <- set_cookies(Headers)].

session_id(Headers) ->
    [Cookie] = session_cookies(Headers),
    "__Host-session_id=" ++ Rest = hd(string:split(Cookie, ";")),
    Rest.

attributes_of(Cookie) ->
    [string:trim(Attribute) || Attribute <- tl(string:split(Cookie, ";", all))].

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 0 ->
            []
    end.
