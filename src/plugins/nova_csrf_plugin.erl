%% @doc CSRF protection plugin for Nova using the synchronizer token pattern.
%%
%% Generates a random token per session, stores it server-side, and validates
%% it on state-changing requests (POST, PUT, PATCH, DELETE).
%%
%% <b>Important:</b> `nova_request_plugin' must run before this plugin so that
%% form params are parsed into the `params' key of the request map.
%%
%% == Options ==
%% <ul>
%%   <li>`field_name' — form field name (default `<<"_csrf_token">>')</li>
%%   <li>`header_name' — header name (default `<<"x-csrf-token">>')</li>
%%   <li>`session_key' — session storage key (default `<<"_csrf_token">>')</li>
%%   <li>`excluded_paths' — list of path prefixes to skip (default `[]')</li>
%% </ul>
-module(nova_csrf_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

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
%% @doc
%% Pre-request callback. On safe methods, ensures a CSRF token exists
%% in the session and injects it into the Req map. On unsafe methods,
%% validates the submitted token against the session token.
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
    {stop, nova_plugin:reply(), Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req = #{method := Method, path := Path}, _Env, Options, State) ->
    FieldName = maps:get(field_name, Options, <<"_csrf_token">>),
    HeaderName = maps:get(header_name, Options, <<"x-csrf-token">>),
    SessionKey = maps:get(session_key, Options, <<"_csrf_token">>),
    ExcludedPaths = maps:get(excluded_paths, Options, []),
    case is_safe_method(Method) orelse is_excluded_path(Path, ExcludedPaths) of
        true ->
            handle_safe_request(Req, SessionKey, State);
        false ->
            handle_unsafe_request(Req, SessionKey, FieldName, HeaderName, State)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback. Pass-through.
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Plugin info callback.
%% @end
%%--------------------------------------------------------------------
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
            {ok, Req#{csrf_token => Token}, State};
        {error, _} ->
            %% No session yet (first visit) — generate token and store it
            Token = generate_token(),
            case nova_session:set(Req, SessionKey, Token) of
                ok ->
                    {ok, Req#{csrf_token => Token}, State};
                {error, _} ->
                    %% Session not established yet (no cookie), proceed without token
                    {ok, Req, State}
            end
    end.

handle_unsafe_request(Req, SessionKey, FieldName, HeaderName, State) ->
    case nova_session:get(Req, SessionKey) of
        {ok, SessionToken} ->
            SubmittedToken = get_submitted_token(Req, FieldName, HeaderName),
            case constant_time_compare(SessionToken, SubmittedToken) of
                true ->
                    {ok, Req#{csrf_token => SessionToken}, State};
                false ->
                    reject(Req, State)
            end;
        {error, _} ->
            reject(Req, State)
    end.

reject(Req, State) ->
    {stop, {reply, 403, [{<<"content-type">>, <<"text/plain">>}], <<"Forbidden - CSRF token invalid">>}, Req, State}.

generate_token() ->
    base64:encode(crypto:strong_rand_bytes(32)).

is_safe_method(<<"GET">>) -> true;
is_safe_method(<<"HEAD">>) -> true;
is_safe_method(<<"OPTIONS">>) -> true;
is_safe_method(_) -> false.

is_excluded_path(_Path, []) ->
    false;
is_excluded_path(Path, [Prefix | Rest]) ->
    case binary:match(Path, Prefix) of
        {0, _} -> true;
        _ -> is_excluded_path(Path, Rest)
    end.

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
