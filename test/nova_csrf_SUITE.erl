%%% nova_csrf_plugin with a real session, a real form body and a real view.
-module(nova_csrf_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([first_visit_starts_a_session_with_a_token/1,
         token_is_masked_anew_in_every_response/1,
         token_without_a_mask_is_accepted/1,
         masked_token_of_another_session_is_refused/1,
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

-export([first_visit_gets_a_cookie_and_no_session/1,
         every_response_has_another_token_and_each_is_accepted/1,
         post_without_the_cookie_is_refused/1,
         post_with_the_token_of_another_client_is_refused/1,
         token_that_was_changed_is_refused/1,
         token_that_decodes_short_is_refused_without_a_crash/1,
         token_of_another_secret_is_refused/1,
         token_of_the_previous_secret_is_accepted_while_it_is_listed/1,
         signing_in_moves_the_token_to_the_new_session/1,
         token_stays_good_while_the_session_is_written/1,
         session_token_of_an_open_form_is_accepted/1]).

-export([request_of_another_site_is_refused/1,
         request_of_the_site_itself_is_accepted/1,
         request_of_another_origin_is_refused/1,
         request_of_a_listed_origin_is_accepted/1,
         any_origin_still_needs_the_token/1,
         origins_of_the_plugin_are_the_ones_that_count/1,
         reading_and_excluded_paths_are_open_to_other_sites/1,
         request_of_another_site_does_not_reach_the_security_callback/1]).

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(OTHER_SECRET, ~"another secret of thirty-two bytes or more, for them").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [{group, session}, {group, stateless}].

groups() ->
    Either = [view_receives_the_token,
              post_without_a_token_is_refused,
              post_with_the_token_in_the_header_is_accepted,
              post_with_the_token_in_the_form_is_accepted,
              post_with_a_wrong_token_is_refused,
              other_unsafe_methods_are_refused_too,
              excluded_path_needs_no_token,
              path_next_to_an_excluded_one_needs_a_token,
              request_of_another_site_is_refused,
              request_of_the_site_itself_is_accepted,
              request_of_another_origin_is_refused,
              request_of_a_listed_origin_is_accepted,
              any_origin_still_needs_the_token,
              origins_of_the_plugin_are_the_ones_that_count,
              reading_and_excluded_paths_are_open_to_other_sites,
              request_of_another_site_does_not_reach_the_security_callback],
    [{session, [], [first_visit_starts_a_session_with_a_token,
                    token_is_masked_anew_in_every_response,
                    token_without_a_mask_is_accepted,
                    masked_token_of_another_session_is_refused,
                    post_without_a_session_is_refused,
                    post_with_the_token_of_another_session_is_refused
                    | Either]},
     {stateless, [], [first_visit_gets_a_cookie_and_no_session,
                      every_response_has_another_token_and_each_is_accepted,
                      post_without_the_cookie_is_refused,
                      post_with_the_token_of_another_client_is_refused,
                      token_that_was_changed_is_refused,
                      token_that_decodes_short_is_refused_without_a_crash,
                      token_of_another_secret_is_refused,
                      token_of_the_previous_secret_is_accepted_while_it_is_listed,
                      signing_in_moves_the_token_to_the_new_session,
                      token_stays_good_while_the_session_is_written,
                      session_token_of_an_open_form_is_accepted
                      | Either]}].

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
                           {"/sign-in", fun nova_csrf_pages:sign_in/1, #{methods => [post]}},
                           {"/hooks/incoming", fun nova_csrf_pages:submit/1, #{methods => [post]}},
                           {"/hooks-admin", fun nova_csrf_pages:submit/1, #{methods => [post]}}]},
              #{prefix => "/partners",
                plugins => [{pre_request, nova_csrf_plugin, #{trusted_origins => [~"https://partner.example.com"]}}],
                routes => [{"/submit", fun nova_csrf_pages:submit/1, #{methods => [post]}}]},
              #{prefix => "/members",
                security => fun nova_csrf_pages:secure/1,
                plugins => [{pre_request, nova_csrf_plugin, #{}}],
                routes => [{"/submit", fun nova_csrf_pages:submit/1, #{methods => [post]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    code:purge(nova_csrf_pages_dtl),
    code:delete(nova_csrf_pages_dtl),
    ok.

init_per_group(stateless, Config) ->
    [{csrf_secret, ?SECRET} | Config];
init_per_group(session, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%% The settings are read when Nova starts, which nova_test_settings does
%% over for a case that changes one.
init_per_testcase(_Case, Config) ->
    case proplists:get_value(csrf_secret, Config) of
        undefined -> ok;
        Secret -> nova_test_settings:set(csrf_secret, Secret)
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    nova_test_settings:unset(trusted_origins),
    nova_test_settings:unset(csrf_secret).

first_visit_starts_a_session_with_a_token(Config) ->
    {{"cookie", <<"session_id=", SessionId/binary>>}, Token} = session(Config),
    {ok, <<Stored:44/binary>>} = nova_session_ets:get_value(SessionId, ~"_csrf_token"),
    ?assertEqual(Stored, unmasked(Token)).

%% The token of a session is sent under another mask in every response, so
%% a response that is compressed gives nothing to compare from one request
%% to the next. What is under the mask stays the same.
token_is_masked_anew_in_every_response(Config) ->
    {Cookie, First} = session(Config),
    {200, Second} = request(Config, "GET", "/csrf/token", [Cookie], <<>>),
    ?assertNotEqual(First, Second),
    ?assertEqual(unmasked(First), unmasked(Second)),
    [?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>))
     || Token <- [First, Second, First]].

%% What a page rendered by an earlier release holds.
token_without_a_mask_is_accepted(Config) ->
    {Cookie, Token} = session(Config),
    ?assertEqual({200, ~"submitted"},
                 request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", unmasked(Token)}], <<>>)).

masked_token_of_another_session_is_refused(Config) ->
    {Cookie, Token} = session(Config),
    {_OtherCookie, OtherToken} = session(Config),
    Mask = binary:part(base64:decode(Token, #{mode => urlsafe, padding => false}), 0, 44),
    Remasked = base64:encode(<<Mask/binary, (crypto:exor(Mask, unmasked(OtherToken)))/binary>>,
                             #{mode => urlsafe, padding => false}),
    [?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Wrong}], <<>>))
     || Wrong <- [OtherToken, Remasked, binary:part(Token, 0, 117), <<Token/binary, "A">>, binary:copy(~"!", 118)]].

unmasked(Token) ->
    <<Mask:44/binary, Masked:44/binary>> = base64:decode(Token, #{mode => urlsafe, padding => false}),
    crypto:exor(Mask, Masked).

view_receives_the_token(Config) ->
    {Cookie, _Token} = session(Config),
    {200, Form} = request(Config, "GET", "/csrf/form", [Cookie], <<>>),
    [~"<input name=", ~"_csrf_token", ~" value=", Token, ~">"] = binary:split(Form, ~"\"", [global]),
    ?assertEqual({200, ~"submitted"},
                 request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>)).

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

%% Nothing is kept for a visitor: the token is signed for the id the
%% response hands out in the session cookie.
first_visit_gets_a_cookie_and_no_session(Config) ->
    Sessions = ets:info(nova_session_ets_entries, size),
    {{"cookie", <<"session_id=", SessionId/binary>>}, Token} = session(Config),
    ?assertEqual(64, byte_size(Token)),
    ?assertNot(nova_session_ets:session_exists(SessionId)),
    [session(Config) || _ <- lists:seq(1, 20)],
    ?assertEqual(Sessions, ets:info(nova_session_ets_entries, size)).

every_response_has_another_token_and_each_is_accepted(Config) ->
    {Cookie, First} = session(Config),
    {200, Second} = request(Config, "GET", "/csrf/token", [Cookie], <<>>),
    ?assertNotEqual(First, Second),
    [?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>))
     || Token <- [First, Second, First]].

post_without_the_cookie_is_refused(Config) ->
    {_Cookie, Token} = session(Config),
    {403, Headers, _Body} =
        nova_test_listener:response(?config(port, Config), "POST", "/csrf/submit", [{"x-csrf-token", Token}], <<>>),
    ?assertNot(is_map_key(~"set-cookie", Headers)).

post_with_the_token_of_another_client_is_refused(Config) ->
    {Cookie, _Token} = session(Config),
    {_OtherCookie, OtherToken} = session(Config),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", OtherToken}], <<>>)).

token_that_was_changed_is_refused(Config) ->
    {Cookie, <<First, Rest/binary>> = Token} = session(Config),
    Flipped = <<(case First of $A -> $B; _ -> $A end), Rest/binary>>,
    [?assertMatch({Changed, {403, _}},
                  {Changed, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Changed}], <<>>)})
     || Changed <- [Flipped, binary:part(Token, 0, 63), <<Token/binary, "A">>, binary:copy(~"!", 64),
                    binary:copy(~"A", 64), base64:encode(crypto:strong_rand_bytes(32))]].

%% base64 skips whitespace, so 64 characters need not hold the bytes of a
%% token. The header keeps the spaces a client puts inside it, and a `+` in
%% a form is a space.
token_that_decodes_short_is_refused_without_a_crash(Config) ->
    {Cookie, _Token} = session(Config),
    Spaced = fun(Space) -> [binary:copy(~"A", 10), binary:copy(Space, 44), binary:copy(~"A", 10)] end,
    {Answers, Events} =
        nova_test_log:collect(
          fun() ->
                  [request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Spaced(~" ")}], <<>>),
                   request(Config, "POST", "/csrf/submit",
                           [Cookie, {"content-type", "application/x-www-form-urlencoded"}],
                           ["_csrf_token=", Spaced(~"+")])]
          end),
    ?assertMatch([{403, _}, {403, _}], Answers),
    ?assertEqual([], nova_test_log:errors(Events)).

token_of_another_secret_is_refused(Config) ->
    {Cookie, Token} = session(Config),
    nova_test_settings:set(csrf_secret, ?OTHER_SECRET),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>)).

%% The first secret signs and every secret in the list is accepted, so a
%% form that was open when the secret was changed can still be sent.
token_of_the_previous_secret_is_accepted_while_it_is_listed(Config) ->
    {Cookie, Old} = session(Config),
    nova_test_settings:set(csrf_secret, [?OTHER_SECRET, ?SECRET]),
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Old}], <<>>)),
    {200, New} = request(Config, "GET", "/csrf/token", [Cookie], <<>>),
    nova_test_settings:set(csrf_secret, ?OTHER_SECRET),
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", New}], <<>>)),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Old}], <<>>)).

%% The id a visitor was handed is not one a session is stored under, so
%% signing in moves the client to another id. A token is for the id it was
%% signed for.
signing_in_moves_the_token_to_the_new_session(Config) ->
    {Cookie, Token} = session(Config),
    {200, Headers, ~"signed in"} =
        nova_test_listener:response(?config(port, Config), "POST", "/csrf/sign-in", [Cookie, {"x-csrf-token", Token}], <<>>),
    SignedIn = cookie(Headers),
    ?assertNotEqual(Cookie, SignedIn),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [SignedIn, {"x-csrf-token", Token}], <<>>)),
    {200, New} = request(Config, "GET", "/csrf/token", [SignedIn], <<>>),
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [SignedIn, {"x-csrf-token", New}], <<>>)).

token_stays_good_while_the_session_is_written(Config) ->
    {Cookie, Token} = session(Config),
    {200, Headers, ~"signed in"} =
        nova_test_listener:response(?config(port, Config), "POST", "/csrf/sign-in", [Cookie, {"x-csrf-token", Token}], <<>>),
    SignedIn = cookie(Headers),
    {200, New} = request(Config, "GET", "/csrf/token", [SignedIn], <<>>),
    {200, AgainHeaders, ~"signed in"} =
        nova_test_listener:response(?config(port, Config), "POST", "/csrf/sign-in", [SignedIn, {"x-csrf-token", New}], <<>>),
    ?assertNot(is_map_key(~"set-cookie", AgainHeaders)),
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [SignedIn, {"x-csrf-token", New}], <<>>)).

%% A form that was rendered while the tokens were kept in the session store
%% holds the token of its session, which is still there to compare with.
session_token_of_an_open_form_is_accepted(Config) ->
    nova_test_settings:unset(csrf_secret),
    {Cookie, SessionToken} = session(Config),
    nova_test_settings:set(csrf_secret, ?SECRET),
    [?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token}], <<>>))
     || Token <- [SessionToken, unmasked(SessionToken)]],
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", ~"wrong"}], <<>>)).

%% A browser says with Sec-Fetch-Site where the page that sends a request
%% is, and with Origin which one it is. The token is asked for as well.
request_of_another_site_is_refused(Config) ->
    [?assertMatch({Site, {403, _}}, {Site, submit(Config, [{"sec-fetch-site", Site}])})
     || Site <- ["cross-site", "same-site"]],
    ?assertMatch({403, _}, submit(Config, [{"sec-fetch-site", "cross-site"}, {"origin", "http://localhost"}])).

request_of_the_site_itself_is_accepted(Config) ->
    [?assertEqual({Headers, {200, ~"submitted"}}, {Headers, submit(Config, Headers)})
     || Headers <- [[{"sec-fetch-site", "same-origin"}],
                    [{"sec-fetch-site", "same-origin"}, {"origin", "http://localhost"}],
                    [{"sec-fetch-site", "none"}],
                    [{"origin", "http://localhost"}],
                    []]].

%% What a browser without Sec-Fetch-Site sends.
request_of_another_origin_is_refused(Config) ->
    [?assertMatch({Origin, {403, _}}, {Origin, submit(Config, [{"origin", Origin}])})
     || Origin <- ["https://evil.example", "http://localhost.evil.example", "null"]].

request_of_a_listed_origin_is_accepted(Config) ->
    nova_test_settings:set(trusted_origins, [~"https://app.example.com"]),
    [?assertEqual({Headers, {200, ~"submitted"}}, {Headers, submit(Config, Headers)})
     || Headers <- [[{"sec-fetch-site", "cross-site"}, {"origin", "https://app.example.com"}],
                    [{"sec-fetch-site", "same-site"}, {"origin", "https://app.example.com"}],
                    [{"origin", "https://app.example.com"}],
                    [{"sec-fetch-site", "same-origin"}]]],
    [?assertMatch({Headers, {403, _}}, {Headers, submit(Config, Headers)})
     || Headers <- [[{"sec-fetch-site", "cross-site"}, {"origin", "https://evil.example"}],
                    [{"sec-fetch-site", "cross-site"}],
                    [{"origin", "http://localhost"}]]].

any_origin_still_needs_the_token(Config) ->
    nova_test_settings:set(trusted_origins, any),
    Other = [{"sec-fetch-site", "cross-site"}, {"origin", "https://evil.example"}],
    ?assertEqual({200, ~"submitted"}, submit(Config, Other)),
    {Cookie, _Token} = session(Config),
    ?assertMatch({403, _}, request(Config, "POST", "/csrf/submit", [Cookie | Other], <<>>)).

origins_of_the_plugin_are_the_ones_that_count(Config) ->
    {Cookie, Token} = session(Config),
    Partner = fun(Origin) ->
                      request(Config, "POST", "/partners/submit",
                              [Cookie, {"x-csrf-token", Token}, {"sec-fetch-site", "cross-site"}, {"origin", Origin}],
                              <<>>)
              end,
    ?assertEqual({200, ~"submitted"}, Partner("https://partner.example.com")),
    ?assertMatch({403, _}, Partner("https://app.example.com")),
    nova_test_settings:set(trusted_origins, [~"https://app.example.com"]),
    ?assertEqual({200, ~"submitted"}, Partner("https://partner.example.com")),
    ?assertMatch({403, _}, Partner("https://app.example.com")).

reading_and_excluded_paths_are_open_to_other_sites(Config) ->
    Other = [{"sec-fetch-site", "cross-site"}, {"origin", "https://evil.example"}],
    ?assertMatch({200, _}, request(Config, "GET", "/csrf/token", Other, <<>>)),
    ?assertEqual({200, ~"submitted"}, request(Config, "POST", "/csrf/hooks/incoming", Other, <<>>)).

%% The headers are all it takes, so the request is turned away ahead of the
%% security callback and before anything of its body is read.
request_of_another_site_does_not_reach_the_security_callback(Config) ->
    {Cookie, Token} = session(Config),
    Submit = fun(Headers) ->
                     request(Config, "POST", "/members/submit", [Cookie, {"x-csrf-token", Token} | Headers], <<>>)
             end,
    register(nova_csrf_observer, self()),
    try
        ?assertMatch({403, _}, Submit([{"sec-fetch-site", "cross-site"}])),
        ?assertEqual([], observed()),
        ?assertEqual({200, ~"submitted"}, Submit([{"sec-fetch-site", "same-origin"}])),
        ?assertEqual([security], observed())
    after
        unregister(nova_csrf_observer)
    end.

observed() ->
    receive
        {observed, What} -> [What | observed()]
    after 200 ->
        []
    end.

submit(Config, Headers) ->
    {Cookie, Token} = session(Config),
    request(Config, "POST", "/csrf/submit", [Cookie, {"x-csrf-token", Token} | Headers], <<>>).

session(Config) ->
    {200, Headers, Token} = nova_test_listener:response(?config(port, Config), "GET", "/csrf/token", []),
    {cookie(Headers), Token}.

cookie(Headers) ->
    [SessionCookie | _Attributes] = binary:split(maps:get(~"set-cookie", Headers), ~";"),
    {"cookie", SessionCookie}.

request(Config, Method, Path, Headers, Body) ->
    {Status, _Headers, ResponseBody} =
        nova_test_listener:response(?config(port, Config), Method, Path, Headers, Body),
    {Status, ResponseBody}.
