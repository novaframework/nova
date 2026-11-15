-module(nova_test_helper).
-export([
         mock_req/2,
         reply/2,
         with_header/3,
         with_json_body/2,
         with_bindings/2,
         with_body/2,
         with_qs/2,
         with_content_type/2,
         setup_nova_env/0,
         cleanup_nova_env/1,
         make_tmp_dir/1
        ]).

%% A new directory for the files of a test. Its name is one no other run
%% makes: several of them share /tmp, and every one of them counts its
%% unique integers from the same start.
-spec make_tmp_dir(Name :: string()) -> file:filename().
make_tmp_dir(Name) ->
    Unique = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(6))),
    Dir = filename:join("/tmp", lists:flatten([Name, $_, os:getpid(), $_, Unique])),
    ok = file:make_dir(Dir),
    Dir.

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
    Body = iolist_to_binary(json:encode(JSON)),
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
    application:unset_env(nova, json_lib),
    Prev.

-spec cleanup_nova_env(undefined | {ok, atom()}) -> ok.
cleanup_nova_env(undefined) ->
    application:unset_env(nova, bootstrap_application),
    ok;
cleanup_nova_env({ok, Val}) ->
    application:set_env(nova, bootstrap_application, Val),
    ok.

%% Stands in for cowboy_req:reply/2 where a test mocks it. It refuses what
%% Cowboy refuses: a second reply, and a body with a 204 or a 304. Unlike
%% Cowboy it leaves the response in the request and adds the status, for the
%% test to look at.
-spec reply(cowboy:http_status(), map()) -> map().
reply(_Status, #{has_sent_resp := _}) ->
    exit({response_error, response_already_sent, 'The final response has already been sent.'});
reply(Status, Req) when Status =:= 204; Status =:= 304 ->
    case maps:get(resp_body, Req, <<>>) of
        Body when is_binary(Body); is_list(Body) ->
            case iolist_size(Body) of
                0 -> Req#{resp_status_code => Status, has_sent_resp => true};
                _ -> exit({response_error, payload_too_large,
                           '204 and 304 responses must not include a body. (RFC7230 3.3)'})
            end;
        _Sendfile ->
            exit({response_error, payload_too_large,
                  '204 and 304 responses must not include a body. (RFC7230 3.3)'})
    end;
reply(Status, Req) ->
    Req#{resp_status_code => Status, has_sent_resp => true}.
