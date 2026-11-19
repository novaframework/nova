-module(nova_csrf_plugin).
-moduledoc """
CSRF protection plugin for Nova using the synchronizer token pattern.

Generates a random token per session, stores it server-side, and validates it on state-changing requests (POST, PUT, PATCH, DELETE).

__Important:__`nova_request_plugin` must run before this plugin so that form params are parsed into the `params` key of the request map.

### Options

* `field_name` — form field name (default `<<"_csrf_token">>`)
* `header_name` — header name (default `<<"x-csrf-token">>`)
* `session_key` — session storage key (default `<<"_csrf_token">>`)
* `excluded_paths` — list of path prefixes to skip (default `[]`)
""".
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-define(NONCE_BYTES, 16).
-define(SIGNATURE_BYTES, 32).

-ifdef(TEST).
-export([
         generate_token/0,
         is_safe_method/1,
         is_excluded_path/2,
         get_submitted_token/3,
         constant_time_compare/2
        ]).
-endif.

%%--------------------------------------------------------------------
-spec tokens() -> {stateless, [binary()]} | session.
tokens() ->
    case nova_settings:value(csrf_token) =/= session andalso nova_secret:keys(csrf_token) of
        [_ | _] = Keys -> {stateless, Keys};
        _NoSecret -> session
    end.

-doc "Pre-request callback. On safe methods, puts a CSRF token into the Req map. On unsafe methods, validates the submitted token.".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req = #{method := Method, path := Path}, _Env, Options, State) ->
    FieldName = maps:get(field_name, Options, <<"_csrf_token">>),
    HeaderName = maps:get(header_name, Options, <<"x-csrf-token">>),
    SessionKey = maps:get(session_key, Options, <<"_csrf_token">>),
    ExcludedPaths = maps:get(excluded_paths, Options, []),
    Safe = is_safe_method(Method) orelse is_excluded_path(Path, ExcludedPaths),
    case tokens() of
        session when Safe ->
            handle_safe_request(Req, SessionKey, State);
        session ->
            handle_unsafe_request(Req, SessionKey, FieldName, HeaderName, State);
        {stateless, Keys} when Safe ->
            {ok, with_signed_token(nova_session:client_id(Req), Keys, Req), State};
        {stateless, Keys} ->
            Presented = nova_session:presented_session_id(Req),
            Submitted = get_submitted_token(Req, FieldName, HeaderName),
            case is_signed_for(Presented, Submitted, Keys) orelse is_session_token(Req, SessionKey, Submitted) of
                true -> {ok, with_signed_token({ok, Presented}, Keys, Req), State};
                false -> reject(Req, State)
            end
    end.

%%--------------------------------------------------------------------
-doc "Post-request callback. Pass-through.".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
-doc "Plugin info callback.".
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => <<"Nova CSRF Plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"CSRF protection using synchronizer token pattern.">>,
      options => [
                  {field_name, <<"Form field name for CSRF token (default: _csrf_token)">>},
                  {header_name, <<"Header name for CSRF token (default: x-csrf-token)">>},
                  {session_key, <<"Session key for CSRF token (default: _csrf_token)">>},
                  {excluded_paths, <<"List of path prefixes to exclude from CSRF protection">>}
                 ]}.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

handle_safe_request(Req, SessionKey, State) ->
    case nova_session:get(Req, SessionKey) of
        {ok, Token} ->
            {ok, Req#{csrf_token => masked(Token)}, State};
        {error, _} ->
            Token = generate_token(),
            case nova_session:set_provisional(Req, SessionKey, Token) of
                ok ->
                    {ok, Req#{csrf_token => masked(Token)}, State};
                {error, _} ->
                    %% Session not established yet (no cookie), proceed without token
                    {ok, Req, State}
            end
    end.

handle_unsafe_request(Req, SessionKey, FieldName, HeaderName, State) ->
    case nova_session:get(Req, SessionKey) of
        {ok, SessionToken} ->
            case is_token(SessionToken, get_submitted_token(Req, FieldName, HeaderName)) of
                true ->
                    {ok, Req#{csrf_token => masked(SessionToken)}, State};
                false ->
                    reject(Req, State)
            end;
        {error, _} ->
            reject(Req, State)
    end.

-spec masked(Token :: term()) -> term().
masked(Token) when is_binary(Token) ->
    Pad = crypto:strong_rand_bytes(byte_size(Token)),
    base64:encode(<<Pad/binary, (crypto:exor(Pad, Token))/binary>>, #{mode => urlsafe, padding => false});
masked(Token) ->
    Token.

-spec is_token(SessionToken :: term(), Submitted :: term()) -> boolean().
is_token(SessionToken, Submitted) ->
    constant_time_compare(SessionToken, Submitted) orelse constant_time_compare(SessionToken, unmasked(Submitted)).

-spec unmasked(Submitted :: term()) -> binary() | undefined.
unmasked(Submitted) ->
    case decoded(Submitted) of
        Decoded when is_binary(Decoded), byte_size(Decoded) rem 2 =:= 0 ->
            Size = byte_size(Decoded) div 2,
            <<Pad:Size/binary, Masked:Size/binary>> = Decoded,
            crypto:exor(Pad, Masked);
        _NotMasked ->
            undefined
    end.

-spec decoded(Submitted :: term()) -> binary() | undefined.
decoded(Submitted) when is_binary(Submitted) ->
    Encoding = #{mode => urlsafe, padding => false},
    try base64:decode(Submitted, Encoding) of
        Decoded ->
            case constant_time_compare(base64:encode(Decoded, Encoding), Submitted) of
                true -> Decoded;
                false -> undefined
            end
    catch
        error:_NotBase64 -> undefined
    end;
decoded(_Submitted) ->
    undefined.

-spec with_signed_token(SessionId :: {ok, term()} | {error, term()}, Keys :: [binary()], Req) -> Req
              when Req :: cowboy_req:req().
with_signed_token({ok, SessionId}, [Key | _], Req) when is_binary(SessionId) ->
    Nonce = crypto:strong_rand_bytes(?NONCE_BYTES),
    Signed = <<Nonce/binary, (signature(Key, SessionId, Nonce))/binary>>,
    Req#{csrf_token => base64:encode(Signed, #{mode => urlsafe, padding => false})};
with_signed_token(_NoSessionId, _Keys, Req) ->
    Req.

-spec signature(Key :: binary(), SessionId :: binary(), Nonce :: binary()) -> binary().
signature(Key, SessionId, Nonce) ->
    crypto:mac(hmac, sha256, Key, [~"nova_csrf_plugin", SessionId, Nonce]).

-spec is_signed_for(SessionId :: term(), Submitted :: term(), Keys :: [binary()]) -> boolean().
is_signed_for(SessionId, <<Token:64/binary>>, Keys) when is_binary(SessionId) ->
    case decoded(Token) of
        <<Nonce:?NONCE_BYTES/binary, Signature:?SIGNATURE_BYTES/binary>> ->
            [] =/= [Key || Key <- Keys, constant_time_compare(Signature, signature(Key, SessionId, Nonce))];
        _NotSigned ->
            false
    end;
is_signed_for(_SessionId, _Submitted, _Keys) ->
    false.

-spec is_session_token(Req :: cowboy_req:req(), SessionKey :: binary(), Submitted :: term()) -> boolean().
is_session_token(Req, SessionKey, Submitted) ->
    case nova_session:get(Req, SessionKey) of
        {ok, SessionToken} -> is_token(SessionToken, Submitted);
        {error, _} -> false
    end.

reject(Req, State) ->
    {stop, {reply, 403, [{<<"content-type">>, <<"text/plain">>}], <<"Forbidden - CSRF token invalid">>}, Req, State}.

generate_token() ->
    base64:encode(crypto:strong_rand_bytes(32)).

is_safe_method(<<"GET">>) -> true;
is_safe_method(<<"HEAD">>) -> true;
is_safe_method(<<"OPTIONS">>) -> true;
is_safe_method(_) -> false.

-spec is_excluded_path(Path :: binary(), Prefixes :: [iodata()]) -> boolean().
is_excluded_path(Path, Prefixes) ->
    Segments = routed_segments(Path),
    [] =/= [Prefix || Prefix <- Prefixes,
                      lists:prefix(routed_segments(iolist_to_binary(Prefix)), Segments)].

routed_segments(Path) ->
    binary:split(nova_router:canonical_path(Path), ~"/", [global, trim_all]).

get_submitted_token(Req, FieldName, HeaderName) ->
    case cowboy_req:header(HeaderName, Req) of
        undefined ->
            case Req of
                #{params := Params} when is_map(Params) ->
                    maps:get(FieldName, Params, undefined);
                _ ->
                    undefined
            end;
        HeaderValue ->
            HeaderValue
    end.

constant_time_compare(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_compare(_, _) ->
    false.
