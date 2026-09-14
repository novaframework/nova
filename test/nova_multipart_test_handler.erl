%% Test-only nova_multipart_handler: accumulates a part in state and notifies
%% InitArgs `owner' at init/abort so tests can assert on the lifecycle.
-module(nova_multipart_test_handler).
-behaviour(nova_multipart_handler).

-export([init/2, handle_data/2, handle_end/1, handle_abort/2]).

init(_PartInfo, #{reject_init := Reason}) ->
    {reject, Reason};
init(_PartInfo, #{error_init := Reason}) ->
    {error, Reason};
init(PartInfo, #{owner := Owner} = InitArgs) ->
    Owner ! {handler_init, PartInfo},
    {ok, #{owner => Owner, body => <<>>,
           reject_data => maps:get(reject_data, InitArgs, false),
           raise_data => maps:get(raise_data, InitArgs, false),
           bad_return_data => maps:get(bad_return_data, InitArgs, false),
           reject_end => maps:get(reject_end, InitArgs, false)}}.

handle_data(_Data, #{raise_data := true}) ->
    error(boom);
handle_data(_Data, #{bad_return_data := true}) ->
    ok;
handle_data(_Data, #{reject_data := true}) ->
    {error, chunk_rejected};
handle_data(Data, #{body := Body} = State) ->
    {ok, State#{body => <<Body/binary, Data/binary>>}}.

handle_end(#{reject_end := true}) ->
    {error, disk_full};
handle_end(#{body := Body}) ->
    {ok, #{body => Body}}.

handle_abort(Reason, #{owner := Owner}) ->
    Owner ! {handler_abort, Reason},
    ok.
