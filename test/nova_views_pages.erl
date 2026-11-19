%%% The controllers of nova_views_SUITE.
-module(nova_views_pages).

-export([view_tuple/1, default_view/1, with_request/1, other_view/1, with_options/1, with_one_header/1,
         with_header_list/1, no_template/1, proplist/1, string_view/1, named_view/1, links/1]).

view_tuple(_Req) ->
    {view, #{title => ~"view tuple"}}.

default_view(_Req) ->
    {ok, #{title => ~"default"}}.

with_request(Req) ->
    {ok, #{title => ~"request"}, #{}, cowboy_req:set_resp_header(~"x-from-controller", ~"yes", Req)}.

other_view(_Req) ->
    {ok, #{title => ~"named"}, #{view => nova_views_other}}.

with_options(_Req) ->
    {ok, #{title => ~"<b>created</b>"}, #{status_code => 201,
                                         headers => #{~"content-type" => ~"text/html; charset=utf-8",
                                                      ~"x-view" => ~"yes"}}}.

with_one_header(_Req) ->
    {ok, #{title => ~"one header"}, #{headers => #{~"cache-control" => ~"no-store"}}}.

with_header_list(_Req) ->
    {ok, #{title => ~"header list"}, #{headers => [{~"cache-control", ~"no-store"}]}}.

no_template(_Req) ->
    {ok, #{}, #{view => nova_views_missing}}.

proplist(_Req) ->
    {ok, [{title, ~"proplist"}]}.

string_view(_Req) ->
    {ok, #{title => ~"string"}, #{view => "nova_views_other"}}.

named_view(Req) ->
    #{view := View} = cowboy_req:match_qs([view], Req),
    {ok, #{title => ~"named in the request"}, #{view => binary_to_list(View)}}.

links(_Req) ->
    {ok, #{}, #{view => nova_views_links}}.
