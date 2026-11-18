%%% Skips the cases that do not hold on the server the suites run on, each
%%% with the reason. The suites run on Cowboy unless NOVA_TEST_SERVER names
%%% another server package:
%%%
%%%     NOVA_TEST_SERVER=nova_roadrunner rebar3 ct
%%%
%%% What is listed here is what an application that moves to that server
%%% gets differently, so a case is added with the reason and not left to
%%% fail.
-module(nova_server_cth).

-export([init/2, pre_init_per_suite/3, pre_init_per_testcase/4]).
-export([differs/3]).

-spec init(term(), term()) -> {ok, module()}.
init(_Id, _Opts) ->
    {ok, nova_test_listener:server(#{})}.

-spec pre_init_per_suite(module(), Config, module()) -> {Config | {skip, string()}, module()}
              when Config :: list().
pre_init_per_suite(Suite, Config, Server) ->
    skip_or(differs(Server, Suite, all), Config, Server).

-spec pre_init_per_testcase(module(), atom(), Config, module()) -> {Config | {skip, string()}, module()}
              when Config :: list().
pre_init_per_testcase(Suite, Case, Config, Server) ->
    skip_or(differs(Server, Suite, Case), Config, Server).

skip_or(false, Config, Server) -> {Config, Server};
skip_or(Reason, _Config, Server) -> {{skip, Reason}, Server}.

%% Whether a case, or with `all` a whole suite, does not hold on a server.
-spec differs(module(), module(), atom()) -> false | string().
differs(nova_cowboy, _Suite, _Case) ->
    false;
differs(nova_roadrunner, Suite, Case) ->
    case roadrunner(Suite, Case) of
        false -> false;
        Reason -> "On Roadrunner " ++ reason(Reason)
    end;
differs(_Server, _Suite, _Case) ->
    false.

reason(cleartext_http2) ->
    "HTTP/2 without TLS has a listener of its own, and this case speaks it to one that serves HTTP/1.1";
reason(websocket_http2) ->
    "WebSockets are served over HTTP/1.1 only";
reason(body_deadline) ->
    "a request body has the time its listener gives it (request_timeout, min_bytes_per_second), "
        "not the time one read of it is given";
reason(methods) ->
    "a method is made of uppercase letters, and a request with another is answered 400";
reason(stream_closes) ->
    "an HTTP/1.1 connection is closed after a streamed response";
reason(cowboy) ->
    "this is about Cowboy itself: one of its options or its listeners".

roadrunner(nova_stream_idle_timeout_SUITE, all) -> cowboy;
roadrunner(nova_cowboy_advisory_SUITE, all) -> cowboy;
roadrunner(nova_boot_SUITE, listener_of_another_owner_fails_the_boot) -> cowboy;
roadrunner(nova_boot_SUITE, listener_with_tls_is_not_told_about_the_session_cookie) -> cowboy;
roadrunner(nova_route_listeners_SUITE, route_versions) -> cleartext_http2;
roadrunner(nova_route_listeners_SUITE, group_versions) -> cleartext_http2;
roadrunner(nova_canonical_path_SUITE, path_without_a_leading_slash_is_refused) -> cleartext_http2;
roadrunner(nova_own_reply_SUITE, second_answer_over_http2) -> cleartext_http2;
roadrunner(nova_upload_stall_SUITE, stalled_upload_over_http2_is_answered_and_the_connection_goes_on) ->
    cleartext_http2;
roadrunner(nova_ws_upgrade_SUITE, Case) ->
    case lists:suffix("_over_http2", atom_to_list(Case)) of
        true -> websocket_http2;
        false -> false
    end;
roadrunner(nova_upload_stall_SUITE, stalled_upload_is_answered_at_the_chunk_timeout) -> body_deadline;
roadrunner(nova_upload_stall_SUITE, trickled_upload_is_answered_at_the_read_timeout) -> body_deadline;
roadrunner(nova_upload_stall_SUITE, timed_out_upload_closes_the_connection) -> body_deadline;
roadrunner(nova_request_error_SUITE, body_a_controller_waits_too_long_for_is_a_408) -> body_deadline;
roadrunner(nova_route_methods_SUITE, extension_method_is_served) -> methods;
roadrunner(nova_route_methods_SUITE, binary_and_string_keep_their_case) -> methods;
roadrunner(nova_streamed_response_SUITE, next_request_is_served_after_a_crash_in_a_stream) -> stream_closes;
roadrunner(_Suite, _Case) -> false.
