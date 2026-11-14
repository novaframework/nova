%%% Controllers that answer with a template, rendered over a real listener.
-module(nova_views_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ok_renders_the_view_named_after_the_controller/1,
         view_tuple_renders_the_same_view/1,
         variables_can_be_a_proplist/1,
         view_option_names_another_template/1,
         view_option_can_be_a_string/1,
         status_code_and_headers_options_are_used/1,
         request_from_the_controller_is_used/1,
         missing_template_is_a_404/1,
         template_output_is_escaped/1,
         url_tag_adds_the_prefix_of_a_mounted_application/1,
         url_tag_of_an_unknown_application_is_a_hash/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [ok_renders_the_view_named_after_the_controller,
     view_tuple_renders_the_same_view,
     variables_can_be_a_proplist,
     view_option_names_another_template,
     view_option_can_be_a_string,
     status_code_and_headers_options_are_used,
     request_from_the_controller_is_used,
     missing_template_is_a_404,
     template_output_is_escaped,
     url_tag_adds_the_prefix_of_a_mounted_application,
     url_tag_of_an_unknown_application_is_a_hash].

init_per_suite(Config) ->
    ok = compile_view(nova_views_pages_dtl, ~"<h1>{{ title }}</h1>"),
    ok = compile_view(nova_views_other_dtl, ~"<p>other: {{ title }}</p>"),
    ok = compile_view(nova_views_links_dtl,
                      ~"""
                      <a href="{% url "/page" application="nova_test_sub_app" %}">mounted</a>
                      <a href="{% url "/page" application="nova_test_app" %}">host</a>
                      <a href="{% url "/page" application="no_such_application" %}">unknown</a>
                      """),
    Page = fun(Path, Callback) -> {Path, Callback, #{methods => [get]}} end,
    Routes = [#{prefix => "/views",
                plugins => [],
                routes => [Page("/default", fun nova_views_pages:default_view/1),
                           Page("/view-tuple", fun nova_views_pages:view_tuple/1),
                           Page("/proplist", fun nova_views_pages:proplist/1),
                           Page("/other", fun nova_views_pages:other_view/1),
                           Page("/string", fun nova_views_pages:string_view/1),
                           Page("/options", fun nova_views_pages:with_options/1),
                           Page("/request", fun nova_views_pages:with_request/1),
                           Page("/no-template", fun nova_views_pages:no_template/1),
                           Page("/links", fun nova_views_pages:links/1)]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    [begin code:purge(View), code:delete(View) end
     || View <- [nova_views_pages_dtl, nova_views_other_dtl, nova_views_links_dtl]],
    ok.

compile_view(Module, Template) ->
    Options = [{out_dir, false},
               {libraries, [{nova_erlydtl_inventory, nova_erlydtl_inventory}]},
               {default_libraries, [nova_erlydtl_inventory]}],
    case erlydtl:compile_template(Template, Module, Options) of
        {ok, Module} -> ok;
        {ok, Module, _Warnings} -> ok
    end.

ok_renders_the_view_named_after_the_controller(Config) ->
    {Status, Headers, Body} = response(Config, "/views/default"),
    ?assertEqual({200, ~"<h1>default</h1>"}, {Status, Body}),
    ?assertEqual(~"text/html", maps:get(~"content-type", Headers)).

view_tuple_renders_the_same_view(Config) ->
    ?assertMatch({200, _, ~"<h1>view tuple</h1>"}, response(Config, "/views/view-tuple")).

variables_can_be_a_proplist(Config) ->
    ?assertMatch({200, _, ~"<h1>proplist</h1>"}, response(Config, "/views/proplist")).

view_option_names_another_template(Config) ->
    ?assertMatch({200, _, ~"<p>other: named</p>"}, response(Config, "/views/other")).

view_option_can_be_a_string(Config) ->
    ?assertMatch({200, _, ~"<p>other: string</p>"}, response(Config, "/views/string")).

status_code_and_headers_options_are_used(Config) ->
    {Status, Headers, _Body} = response(Config, "/views/options"),
    ?assertEqual(201, Status),
    ?assertEqual(~"text/html; charset=utf-8", maps:get(~"content-type", Headers)),
    ?assertEqual(~"yes", maps:get(~"x-view", Headers)).

request_from_the_controller_is_used(Config) ->
    {Status, Headers, Body} = response(Config, "/views/request"),
    ?assertEqual({200, ~"<h1>request</h1>"}, {Status, Body}),
    ?assertEqual(~"yes", maps:get(~"x-from-controller", Headers)).

missing_template_is_a_404(Config) ->
    ?assertMatch({404, _, _}, response(Config, "/views/no-template")).

template_output_is_escaped(Config) ->
    ?assertMatch({201, _, ~"<h1>&lt;b&gt;created&lt;/b&gt;</h1>"}, response(Config, "/views/options")).

url_tag_adds_the_prefix_of_a_mounted_application(Config) ->
    {200, _Headers, Body} = response(Config, "/views/links"),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<a href=\"/mounted/page\">mounted</a>">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<a href=\"/page\">host</a>">>)).

url_tag_of_an_unknown_application_is_a_hash(Config) ->
    {200, _Headers, Body} = response(Config, "/views/links"),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<a href=\"#\">unknown</a>">>)).

response(Config, Path) ->
    nova_test_listener:response(?config(port, Config), "GET", Path, []).
