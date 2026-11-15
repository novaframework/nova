-module(nova_csrf_plugin).
-moduledoc """
CSRF protection plugin for Nova.

A page gets a token in `csrf_token` of the request, which a view receives as the variable `csrf_token`. A state-changing request (POST, PUT, PATCH, DELETE) has to send it back, in the form field or the header, and is answered `403` when it does not.

__Important:__ `nova_request_plugin` must run before this plugin so that form params are parsed into the `params` key of the request map.

## Where the token comes from

With `csrf_secret` set in the `nova` environment the token is signed and nothing is stored for it. It is an HMAC-SHA256, under the secret, over the id in the session cookie of the client and 16 random bytes, so every response carries another token and each of them is good for as long as the client holds that cookie and the secret is in use. A visitor without a session cookie is handed one by the response (`nova_session:client_id/1`); no session is started. A request that changes state is refused when it does not send the cookie the token was signed for. Signing in moves a visitor to another session id, and the pages rendered after that carry tokens for the new one.

```erlang
{nova, [
    {csrf_secret, ~"at least 32 bytes that every node of the system is given"}
]}
```

The secret has to be the same on every node and across restarts. A list of secrets signs with the first and accepts all of them: put the new secret in front of the old one to change it, and take the old one out once the pages that were open have been sent.

Without `csrf_secret` the token is a random value kept in the session, as in earlier releases: a visitor without a session gets one that holds the token (`nova_session:set_provisional/3`). A page gets it under a mask that is new for every response, so the same token never looks the same twice in a compressed response. The token without a mask, which is what a page rendered by an earlier release holds, is accepted as well. The boot says so once. Set `{csrf_token, session}` to say that this is intended, or `{csrf_token, stateless}` to have a boot without a secret fail instead.

After a secret is set, a form that was rendered before still sends the token of its session. It is accepted for as long as that session is there.

## Where the request comes from

A token is asked for whatever else the request says. On top of that, a request that changes state is answered `403` ahead of the security callback, and before anything of its body is read, when the browser says that a page of another site sent it:

- `Sec-Fetch-Site` is `cross-site` or `same-site`, unless its `Origin` is one of a list of trusted origins. `same-origin` and `none`, which is what a request the user made by hand has, are let through.
- Without `Sec-Fetch-Site`, which browsers send to `https` and `localhost` only, the `Origin` is not a trusted one.

A request with neither header is let through: it is not from a browser, or from one too old to say. The trusted origins are `trusted_origins` of the `nova` environment, the setting websocket upgrades go by, unless the plugin is given its own: `same_host` (the default), `same_origin`, or a list such as `[~"https://app.example.com"]` for pages elsewhere that may post here. A list takes the place of `same_host`: a request that has an `Origin` and no `Sec-Fetch-Site` is let through only when that origin is in the list, also when a page of the site itself sent it, so put the origin of the site in the list.

`any` in the `nova` environment opens websocket upgrades to every origin and nothing else: this plugin then checks by `same_host`. To turn the check off and leave a request to its token alone, give the plugin the option itself, `#{trusted_origins => any}`, in the route groups that are meant to be posted to from anywhere.

This is not `allow_origins` of `nova_cors_plugin`, which says which pages may read a response. An origin that is to post here from a browser has to be in both.

### Options

* `field_name` - form field name (default `<<"_csrf_token">>`)
* `header_name` - header name (default `<<"x-csrf-token">>`)
* `session_key` - session storage key (default `<<"_csrf_token">>`)
* `trusted_origins` - the origins a request that changes state may come from, when not those of the `nova` environment: `same_host`, `same_origin`, a list of origins, or `any`, which turns the check off. Any other value stops the boot with `{invalid_setting, trusted_origins, Value}`
* `excluded_paths` - list of path prefixes to skip (default `[]`). A prefix covers whole segments of the path the request is routed on: `<<"/api">>` skips `/api` and `/api/hooks`, not `/apikeys`, and `/api/../admin` is checked as `/admin`
""".
-behaviour(nova_plugin).

-export([
         init/0,
         pre_security/4,
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-include_lib("kernel/include/logger.hrl").

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
-doc false.
-spec init() -> nova:state().
init() ->
    Unset = nova_settings:value(csrf_token) =:= undefined andalso nova_settings:value(csrf_secret) =:= [],
    _ = Unset andalso
        ?LOG_NOTICE(#{msg => ~"csrf_secret is not set, so nova_csrf_plugin keeps its tokens in the session store and starts a session for every visitor. Set csrf_secret for signed tokens that store nothing, or csrf_token to session to say that this is intended",
                      key => csrf_secret, csrf_token => session}),
    #{}.

%% How tokens are made: signed with the secrets, the first of which signs,
%% or kept in the session. Both settings were checked when Nova started.
-spec tokens() -> {stateless, [binary()]} | session.
tokens() ->
    case nova_settings:value(csrf_token) =/= session andalso secrets() of
        [_ | _] = Secrets -> {stateless, Secrets};
        _NoSecret -> session
    end.

-spec secrets() -> [binary()].
secrets() ->
    case nova_settings:value(csrf_secret) of
        Secrets when is_list(Secrets) -> [Secret || Secret <- Secrets, is_binary(Secret)];
        _NoSecrets -> []
    end.

%%--------------------------------------------------------------------
-doc "Stops a request that changes state and that the browser says another site sent with a 403, ahead of the security callback and before anything of its body is read.".
-spec pre_security(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
          {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_security(Req = #{method := Method, path := Path}, _Env, Options, State) ->
    case is_safe_method(Method) orelse is_excluded_path(Path, maps:get(excluded_paths, Options, []))
        orelse is_sent_by_the_site(Req, Options) of
        true ->
            {ok, Req, State};
        false ->
            {stop, {reply, 403, [{~"content-type", ~"text/plain"}], ~"Forbidden - request from another origin"},
             Req, State}
    end.

%% `Sec-Fetch-Site` is the browser saying where the page that sends the
%% request is, and no page can set it. When it says the page is somewhere
%% else, comparing the origin with the host of the request has nothing to
%% add, so only an origin that is listed is let in. A browser that does not
%% send it is taken by its `Origin`, and a client that sends neither is not
%% a page that some other site could have made send the request.
-spec is_sent_by_the_site(Req :: cowboy_req:req(), Options :: map()) -> boolean().
is_sent_by_the_site(Req, Options) ->
    Trusted = trusted_origins(Options),
    Origin = cowboy_req:header(~"origin", Req),
    case cowboy_req:header(~"sec-fetch-site", Req) of
        Site when Site =:= ~"same-origin"; Site =:= ~"none"; Trusted =:= any ->
            true;
        undefined ->
            nova_origin:is_allowed(Origin, Req, Trusted);
        _ElseWhere ->
            is_binary(Origin) andalso is_list(Trusted) andalso nova_origin:is_allowed(Origin, Req, Trusted)
    end.

%% `any` of the nova environment is there to open websockets to pages
%% elsewhere. It takes the option of this plugin to leave a request that
%% changes state to its token alone.
-spec trusted_origins(Options :: map()) -> term().
trusted_origins(#{trusted_origins := Origins}) ->
    Origins;
trusted_origins(_Options) ->
    case nova_settings:value(trusted_origins) of
        any -> same_host;
        Origins -> Origins
    end.

%%--------------------------------------------------------------------
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
        {stateless, Secrets} when Safe ->
            {ok, with_signed_token(nova_session:client_id(Req), Secrets, Req), State};
        {stateless, Secrets} ->
            Presented = nova_session:presented_session_id(Req),
            Submitted = get_submitted_token(Req, FieldName, HeaderName),
            case is_signed_for(Presented, Submitted, Secrets) orelse is_session_token(Req, SessionKey, Submitted) of
                true -> {ok, with_signed_token({ok, Presented}, Secrets, Req), State};
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
                  {trusted_origins, ~"same_host, same_origin, any, or the list of origins that may send unsafe requests"},
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
            %% No session yet (first visit): generate token and store it
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

%% The token of a session is the same for as long as the session lives. Sent
%% as it is, a response that is compressed would give it away a byte at a
%% time to somebody who can make the client ask again and again and choose
%% part of what the page says (BREACH). Every response gets it under a pad
%% of its own instead.
-spec masked(Token :: term()) -> term().
masked(Token) when is_binary(Token) ->
    Pad = crypto:strong_rand_bytes(byte_size(Token)),
    base64:encode(<<Pad/binary, (crypto:exor(Pad, Token))/binary>>, #{mode => urlsafe, padding => false});
masked(Token) ->
    Token.

%% A page rendered by a release that did not mask holds the token itself.
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

%% What a client sent as a token, which may be anything. The decoder skips
%% whitespace, so the bytes cannot be counted from the characters.
-spec decoded(Submitted :: term()) -> binary() | undefined.
decoded(Submitted) when is_binary(Submitted) ->
    try
        base64:decode(Submitted, #{mode => urlsafe, padding => false})
    catch
        error:_NotBase64 -> undefined
    end;
decoded(_Submitted) ->
    undefined.

-spec with_signed_token(SessionId :: {ok, term()} | {error, term()}, Secrets :: [binary()], Req) -> Req
              when Req :: cowboy_req:req().
with_signed_token({ok, SessionId}, [Secret | _], Req) when is_binary(SessionId) ->
    Nonce = crypto:strong_rand_bytes(?NONCE_BYTES),
    Signed = <<Nonce/binary, (signature(Secret, SessionId, Nonce))/binary>>,
    Req#{csrf_token => base64:encode(Signed, #{mode => urlsafe, padding => false})};
with_signed_token(_NoSessionId, _Secrets, Req) ->
    Req.

%% The nonce has a fixed length and comes last, so the session id is what is
%% between it and the label whatever bytes a client put in its cookie.
-spec signature(Secret :: binary(), SessionId :: binary(), Nonce :: binary()) -> binary().
signature(Secret, SessionId, Nonce) ->
    crypto:mac(hmac, sha256, Secret, [~"nova_csrf_plugin", SessionId, Nonce]).

%% 64 characters are the 16 bytes of a nonce and the 32 of its signature,
%% when every one of them is a character of the encoding.
-spec is_signed_for(SessionId :: term(), Submitted :: term(), Secrets :: [binary()]) -> boolean().
is_signed_for(SessionId, <<Token:64/binary>>, Secrets) when is_binary(SessionId) ->
    case decoded(Token) of
        <<Nonce:?NONCE_BYTES/binary, Signature:?SIGNATURE_BYTES/binary>> ->
            [] =/= [Secret || Secret <- Secrets,
                              constant_time_compare(Signature, signature(Secret, SessionId, Nonce))];
        _NotSigned ->
            false
    end;
is_signed_for(_SessionId, _Submitted, _Secrets) ->
    false.

%% The token of a form that was rendered before csrf_secret was set.
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

%% Compared on the segments the router matches on. A byte prefix of the raw
%% path let `/api` cover `/apikeys`, and `/api/../admin` reach `/admin`.
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
