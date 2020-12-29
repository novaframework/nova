-module(nova_prefix_router).
-compile(export_all). %% TODO! Replace after tests

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new() ->
    new(#{}).

new(Options) ->
    #{
      '__options__' => Options,
      '__value__' => #{}
     }.


insert(Route, Value, Tree) when is_binary(Route) andalso
                                is_map(Value) ->
    Routes = split_route(Route),
    insert(Routes, Value, Tree);

insert([<<":", _Name/binary>>|Tl], Value, Tree) ->
    insert([<<"*">>, Tl], Value, Tree);
insert([], Value, Tree = #{'__value__' := OldValues}) ->
    Tree#{'__value__' => merge_maps(OldValues, Value)};
insert([Route|Tl], Value, Tree) ->
    case maps:get(Route, Tree, undefined) of
        undefined ->
            Tree#{Route => insert(Tl, Value, new())};
        Subtree ->
            Tree#{Route => insert(Tl, Value, Subtree)}
    end.

get(Binary, Tree) when is_binary(Binary) ->
    Routes = split_route(Binary),
    get(Routes, Tree);
get([<<":", _Name/binary>>|Tl], Tree) ->
    get([<<"*">>, Tl], Tree);
get([Route|Tl], Tree) ->
    case maps:get(Route, Tree, undefined) of
        Subtree = #{'__value__' := Value} ->
            case Tl of
                [] ->
                    {ok, Value};
                _Routes ->
                    get(Tl, Subtree)
            end;
        _ ->
            %% Check for wildcards
            case maps:get(<<"*">>, Tree, undefined) of
                Subtree = #{'__value__' := Value}->
                    case Tl of
                        [] ->
                            {ok, Value};
                        _ ->
                            get(Tl, Subtree)
                    end;
                _ ->
                    {error, not_found}
            end
    end.

merge_maps(M1, M2) ->
    Keys = sets:to_list(sets:from_list(lists:concat([maps:keys(M1), maps:keys(M2)]))),
    Val = merge_maps(Keys, M1, M2, #{}),
    io:format(user, "Keys: ~p, M1: ~p, M2: ~p, VAL: ~p~n", [Keys, M1, M2, Val]),
    Val.

merge_maps([], _, _, Ack) ->
    Ack;
merge_maps([K|Tl], M1, M2, Ack) ->
    case {maps:get(K, M1, undefined),
          maps:get(K, M2, undefined)} of
        {undefined, undefined} ->
            merge_maps(Tl, M1, M2, Ack);
        {undefined, V} ->
            merge_maps(Tl, M1, M2, Ack#{K => V});
        {V, undefined} ->
            merge_maps(Tl, M1, M2, Ack#{K => V});
        %% We only support lists for now
        {V1, V2} when is_list(V1) andalso
                      is_list(V2) ->
            io:format(user, "CONCAT", []),
            merge_maps(Tl, M1, M2, Ack#{K => lists:concat([V1, V2])});
        {_V1, V2} ->
            merge_maps(Tl, M1, M2, Ack#{K => V2})
    end.

split_route(Bin) when is_binary(Bin) ->
    binary:split(Bin, <<"/">>, [trim_all, global]).

-ifdef(TEST).

create_trie_test() ->
    #{'__options__' := Options} = new(),
    ?assert(#{} =:= Options).

insert_regular_path_test() ->
    T0 = new(),
    T1 = insert(<<"/home">>, #{methods => [get]}, T0),
    ?assertMatch({ok, _}, get(<<"/home">>, T1)).


insert_wildcard_path_test() ->
    T0 = new(),
    T1 = insert(<<"/:home">>, #{methods => [get]}, T0),
    ?assertMatch({ok, _}, get(<<"/testpath">>, T1)).

insert_two_values_test() ->
    T0 = new(),
    T1 = insert(<<"/home">>, #{methods => [get]}, T0),
    T2 = insert(<<"/home">>, #{methods =>[post]}, T1),
    {ok, #{methods := Methods}} = get(<<"/home">>, T2),
    ?assertMatch([get, post], Methods).





-endif.
