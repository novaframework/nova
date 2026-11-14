%%% nova_csrf_plugin with a real session, a real form body and a real view.
-module(nova_csrf_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([first_visit_starts_a_session_with_a_token/1,
         token_stays_the_same_within_a_session/1,
         view_receives_the_token/1,
         post_without_a_token_is_refused/1,
         post_without_a_session_is_refused/1,
         post_with_the_token_in_the_header_is_accepted/1,
         post_with_the_token_in_the_form_is_accepted/1,
         post_with_a_wrong_token_is_refused/1,
         post_with_the_token_of_another_session_is_refused/1,
         other_unsafe_methods_are_refused_too/1,
         excluded_path_needs_no_token/1,
         path_next_to_an_excluded_one_needs_a_token/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [first_visit_starts_a_session_with_a_token,
     token_stays_the_same_within_a_session,
     view_receives_the_token,
     post_without_a_token_is_refused,
     post_without_a_session_is_refused,
     post_with_the_token_in_the_header_is_accepted,
     post_with_the_token_in_the_form_is_accepted,
     post_with_a_wrong_token_is_refused,
     post_with_the_token_of_another_session_is_refused,
     other_unsafe_methods_are_refused_too,
     excluded_path_needs_no_token,
     path_next_to_an_excluded_one_needs_a_token].

init_per_suite(Config) ->
    {ok, nova_csrf_pages_dtl} =
        erlydtl:compile_template(<<"<input name=\"_csrf_token\" value=\"{{ csrf_token }}\">">>,
                                 nova_csrf_pages_dtl, [{out_dir, false}]),
    Routes = [#{prefix => "/csrf",
                plugins => [{pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
                            {pre_request, nova_csrf_plugin, #{excluded_paths => [~"/csrf/hooks"]}}],
                routes => [{"/token", fun nova_csrf_pages:token/1, #{methods => [get]}},
                           {"/form", fun nova_csrf_pages:form/1, #{methods => [get]}},
                           {"/submit", fun nova_csrf_pages:submit/1, #{methods => [post, put, patch, delete]}},
                           {"/hooks/incoming", fun nova_csrf_pages:submit/1, #{methods => [post]}},
                           {"/hooks-admin", fun nova_csrf_pages:submit/1, #{methods => [post]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    code:purge(nova_csrf_pages_dtl),
    code:delete(nova_csrf_pages_dtl),
    ok.

first_visit_starts_a_session_with_a_token(Config) ->
    {200, Headers, Token} = nova_test_listener:response(?config(port, Config), "GET", "/csrf/token", []),
    ?assertMatch(<<"session_id=", _/binary>>, maps:get(~"set-cookie", Headers)),
    ?assertEqual(44, byte_size(Token)).

token_stays_the_same_within_a_session(Config) ->
    {Cookie, Token} = session(Config),
    ?assertEqual({200, Token}, request(Config, "GET", "/csrf/token", [Cookie], <<>>)).

view_receives_the_token(Config) ->
    {Cookie, Token} = session(Config),
    ?assertEqual({200, <<"<input name=\"_csrf_token\" value=\"", Token/binary, "\">">>},
                 request(Config, "GET", "/csrf/form", [Cookie], <<>>)).

post_without_a_token_is_refused(Config) ->
    {Cookie, _Token} = session(Config),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie], <<>>)).

post_without_a_session_is_refused(Config) ->
    {_Cookie, Token} = session(Config),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [{"x-csrf-token", Token}], <<>>)).

post_with_the_token_in_the_header_is_accepted(Config) ->
    {Cookie, Token} = session(Config),
    ?assertEqual({200, ~"submitted"},
                 request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>)).

post_with_the_token_in_the_form_is_accepted(Config) ->
    {Cookie, Token} = session(Config),
    Body = ["name=nova&_csrf_token=", uri_string:quote(Token)],
    ?assertEqual({200, ~"submitted"},
                 request(Config, "POST", "/csrf/submit",
                         [Cookie, {"content-type", "application/x-www-form-urlencoded"}], Body)).

post_with_a_wrong_token_is_refused(Config) ->
    {Cookie, Token} = session(Config),
    Wrong = base64:encode(crypto:strong_rand_bytes(32)),
    ?assertNotEqual(Token, Wrong),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Wrong}], <<>>)),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", ""}], <<>>)).

post_with_the_token_of_another_session_is_refused(Config) ->
    {Cookie, _Token} = session(Config),
    {_OtherCookie, OtherToken} = session(Config),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", OtherToken}], <<>>)).

other_unsafe_methods_are_refused_too(Config) ->
    {Cookie, Token} = session(Config),
    [begin
         ?assertMatch({403, _}, request(Config, Method, "/csrf/submit", [Cookie], <<>>)),
         ?assertEqual({200, ~"submitted"},
                      request(Config, Method, "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>))
     end || Method <- ["PUT", "PATCH", "DELETE"]].

excluded_path_needs_no_token(Config) ->
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/hooks/incoming", [], <<>>)).

path_next_to_an_excluded_one_needs_a_token(Config) ->
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/hooks-admin", [], <<>>)).

session(Config) ->
    {200, Headers, Token} = nova_test_listener:response(?config(port, Config), "GET", "/csrf/token", []),
    [SessionCookie | _Attributes] = binary:split(maps:get(~"set-cookie", Headers), ~";"),
    {{"cookie", SessionCookie}, Token}.

request(Config, Method, Path, Headers, Body) ->
    {Status, _Headers, ResponseBody} =
        nova_test_listener:response(?config(port, Config), Method, Path, Headers, Body),
    {Status, ResponseBody}.
