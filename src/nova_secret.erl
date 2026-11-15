-module(nova_secret).
-moduledoc false.

%%% The keys Nova signs with. `secret_key_base` of the nova environment is
%%% never used as it is: every purpose has a key of its own made from it,
%%% so what is signed for one purpose says nothing for another, and a
%%% purpose can be added without another setting.

-export([keys/1, derived/1, derive/2]).

-type purpose() :: csrf_token | session_id.
-export_type([purpose/0]).

-define(KEY_BYTES, 32).

-doc """
The keys of a purpose, one for each secret in `secret_key_base`, in the
order the secrets are listed: the first signs and every one of them
verifies. Empty when no secret is set.
""".
-spec keys(Purpose :: purpose()) -> [binary()].
keys(Purpose) ->
    case nova_settings:value(secret_key_base) of
        Derived when is_list(Derived) -> [Key || #{Purpose := Key} <- Derived];
        _NoSecret -> []
    end.

-doc "The key of every purpose for one secret, which is what Nova keeps of it.".
-spec derived(Secret :: binary()) -> #{purpose() => binary()}.
derived(Secret) ->
    #{Purpose => derive(Secret, label(Purpose)) || Purpose <- purposes()}.

-doc """
A key of 32 bytes for `Label` under `Secret`: HMAC-SHA-256 of the label and
the byte 1, which is the first block of HKDF-Expand (RFC 5869) with the
secret as the pseudorandom key and the label as `info`.
""".
-spec derive(Secret :: binary(), Label :: binary()) -> binary().
derive(Secret, Label) ->
    <<Key:?KEY_BYTES/binary>> = crypto:mac(hmac, sha256, Secret, [Label, 1]),
    Key.

-spec purposes() -> [purpose()].
purposes() ->
    [csrf_token, session_id].

%% Part of what a key is: one that changes makes every key of its purpose
%% another one, on every node.
-spec label(purpose()) -> binary().
label(csrf_token) -> ~"nova csrf token";
label(session_id) -> ~"nova session id".
