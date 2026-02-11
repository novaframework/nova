-module(nova_test_helper).
-export([
         mock_req/2,
         with_header/3,
         with_json_body/2,
         with_bindings/2,
         with_body/2,
         with_qs/2,
         with_content_type/2,
         setup_nova_env/0,
         cleanup_nova_env/1
        ]).

%% Build a minimal cowboy_req-like map for testing pure handler logic.
%% We avoid depending on nova_test (circular dep) and cowboy internals.
-spec mock_req(Method :: binary(), Path :: binary()) -> map().
mock_req(Method, Path) ->
    #{method => Method,
      path => Path,
      host => <<"localhost">>,
      port => 8080,
      qs => <<>>,
      headers => #{},
      resp_headers => #{},
      pid => self(),
      streamid => 1,
      has_body => false}.

-spec with_header(binary(), binary(), map()) -> map().
with_header(Name, Value, Req = #{headers := Headers}) ->
    Req#{headers => Headers#{Name => Value}}.

-spec with_json_body(map() | binary(), map()) -> map().
with_json_body(JSON, Req) when is_map(JSON) ->
    Body = thoas:encode(JSON),
    with_json_body(Body, Req);
with_json_body(Body, Req) when is_binary(Body) ->
    Req1 = with_content_type(<<"application/json">>, Req),
    Req1#{body => Body, has_body => true}.

-spec with_body(binary(), map()) -> map().
with_body(Body, Req) ->
    Req#{body => Body, has_body => true}.

-spec with_bindings(map(), map()) -> map().
with_bindings(Bindings, Req) ->
    Req#{bindings => Bindings}.

-spec with_qs(binary(), map()) -> map().
with_qs(Qs, Req) ->
    Req#{qs => Qs}.

-spec with_content_type(binary(), map()) -> map().
with_content_type(CT, Req = #{headers := Headers}) ->
    Req#{headers => Headers#{<<"content-type">> => CT}}.

%% Set up minimal app env so nova:get_env/2 works in tests.
%% Returns previous bootstrap_application value for cleanup.
-spec setup_nova_env() -> undefined | {ok, atom()}.
setup_nova_env() ->
    Prev = application:get_env(nova, bootstrap_application),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, environment, dev),
    application:set_env(nova, json_lib, thoas),
    Prev.

-spec cleanup_nova_env(undefined | {ok, atom()}) -> ok.
cleanup_nova_env(undefined) ->
    application:unset_env(nova, bootstrap_application),
    ok;
cleanup_nova_env({ok, Val}) ->
    application:set_env(nova, bootstrap_application, Val),
    ok.
