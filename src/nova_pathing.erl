-module(nova_pathing).
-export([
         lookup/4,
         insert/5
        ]).

-include("../include/nova.hrl").
-include("nova_router.hrl").

-define(BINDING_REGEX(Name), "(?<" ++ erlang:binary_to_list(Name) ++ ">[a-zA-Z0-9]*)").
-define(VALIDATE_PATH_REGEX, "^[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)$").

lookup(_Host, _Path, _Method, []) ->
    {error, not_found};
lookup(Host, Path, Method, [{Host, Tree}]) ->
    Segments = split_segments(Path),
    lookup_elem(Segments, Method, Tree, #path_info{});
lookup(Host, Path, Method, [_|Tl]) ->
    lookup(Host, Path, Method, Tl).


lookup_elem([Segment|Tl], Method, [N = #rt_node{segment = Segment}|_Tl0], PathInfo) ->
    lookup_elem(Tl, Method, N#rt_node.children, PathInfo);
lookup_elem([Segment|Tl], Method, [N = #rt_node{segment = Pattern}|_Tl0], PathInfo) when element(1, Pattern) == re_pattern ->
    case re:run(Segment, Pattern, [{capture, all_names, binary}]) of
        {match, MatchList} ->
            {namelist, Namelist} = re:inspect(Pattern, namelist),
            NameMap = lists:zip(Namelist, MatchList),
            lookup_elem(Tl, Method, N#rt_node.children, PathInfo#path_info{bindings = NameMap});
        _ ->
            lookup_elem(Tl, Method, Tl, PathInfo)
    end;
lookup_elem([<<$?, Rest/binary>>|_Tl], Method, [N = #rt_node{comparator = Method}|_Tl0], PathInfo) ->
    QS = cow_qs:parse_qs(Rest),
    {ok, PathInfo#path_info{value = N#rt_node.value,
                            qs = QS}};
lookup_elem([], Method, [N = #rt_node{comparator = Method0}|_Tl], PathInfo) when Method == Method0 orelse
                                                                                 Method0 == '_' ->
    {ok, PathInfo#path_info{value = N#rt_node.value}};
lookup_elem([_Segment|Tl], Method, Tree, PathInfo) ->
    lookup_elem(Tl, Method, Tree, PathInfo);
lookup_elem([], _Method, [], _PathInfo) ->
    {error, not_found}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface for inserting %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert(_, _, _, _, []) -> [];
insert(Host, Path, Method, Value, [{Host, RoutingTree}|Tl]) ->
    Segments = split_segments(Path),
    [{Host, insert_n(Segments, Method, Value, RoutingTree)}|Tl];
insert(Host, Path, Method, Value, [Hd|Tl]) ->
    [Hd|insert(Host, Path, Method, Value, Tl)].


insert_n([], Method, Value, Siblings) ->
    case lists:any(fun(#rt_node{comparator = Comp}) ->
                           Comp == Method orelse Comp == '_'
                   end, Siblings) of
        true ->
            %% We have a conflicting insertion
            logger:warning("Overwriting rule"),
            lists:map(fun(#rt_node{comparator = M} = N) when M == Method ->
                              N#rt_node{value = Value}
                      end, Siblings);
        _ ->
            %% Just to insert the thing
            [#rt_node{comparator = Method,
                      value = Value}|Siblings]
    end;
insert_n([<<":", Rest/binary>>|Segments], Method, Value, Siblings) ->
    %% Binding
    {ok, MP} = re:compile(?BINDING_REGEX(Rest)),
    [#rt_node{segment = MP,
              children = insert_n(Segments, Method, Value, [])}|Siblings];
insert_n([<<"!!", RegExp/binary>>|Segments], Method, Value, Siblings) ->
    %% Regular expression
    case re:compile(RegExp) of
        {ok, MP} ->
            [#rt_node{segment = MP,
                      children = insert_n(Segments, Method, Value, [])}|Siblings];
        _ ->
            throw({invalid_regexp, RegExp})
    end;
insert_n([Segment|Segments], Method, Value, []) ->

    [#rt_node{segment = Segment, children = insert_n(Segments, Method, Value, [])}];
insert_n([Segment|Segments], Method, Value, [N = #rt_node{segment = Segment}|Tl]) ->
    %% Traverse down
    [N#rt_node{children = insert_n(Segments, Method, Value, N#rt_node.children)}|Tl];
insert_n(Segments, Method, Value, [Hd|Tl]) ->
    [Hd|insert_n(Segments, Method, Value, Tl)].


split_segments(Binary) ->
    lists:reverse(split_segments(Binary, <<>>, [])).

split_segments(<<>>, Ack, Result) ->
    [Ack|Result];
split_segments(<<$?, _Rest/binary>> = QS, Ack, Result) ->
    [QS, Ack | Result];
split_segments(<<$/, Rest/binary>>, <<>>, Result) ->
    split_segments(Rest, <<>>, [<<$/>>|Result]);
split_segments(<<$/, Rest/binary>>, Ack, Result) ->
    split_segments(Rest, <<>>, [Ack|Result]);
split_segments(<<Char, Rest/binary>>, Ack, Result) ->
    split_segments(Rest, <<Ack/binary, Char>>, Result).

%% Eunit
-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

basic_insert_test() ->
    Result = insert('_', <<"/hello/world">>, <<"GET">>, <<"Hello world">>, [{'_', []}]),
    Result0 = insert('_', <<"/hello/world">>, <<"POST">>, <<"Hello world2">>, Result),
    Result1 = insert('_', <<"/hello2/:binding">>, <<"POST">>, <<"bind me">>, Result0),
    Result2 = insert('_', <<"/another/!![a-z]">>, <<"POST">>, <<"result">>, Result1),
    {ok, RegExp} = re:compile("[a-z]"),
    {ok, RegExp2} = re:compile(?BINDING_REGEX(<<"binding">>)),
    Expected = [{'_', [#rt_node{segment = <<"/">>,
                                children = [
                                            #rt_node{segment = <<"hello">>,
                                                     children = [
                                                                 #rt_node{segment = <<"world">>,
                                                                          children = [
                                                                                      #rt_node{segment = <<>>,
                                                                                               value = <<"Hello world2">>,
                                                                                               comparator = <<"POST">>},
                                                                                      #rt_node{segment = <<>>,
                                                                                               value = <<"Hello world">>,
                                                                                               comparator = <<"GET">>}
                                                                                     ]}
                                                                ]},
                                            #rt_node{segment = <<"hello2">>,
                                                     children = [
                                                                 #rt_node{segment = RegExp2,
                                                                          children = [
                                                                                      #rt_node{segment = <<>>,
                                                                                               value = <<"bind me">>,
                                                                                               comparator = <<"POST">>}
                                                                                     ]}
                                                                ]},
                                            #rt_node{segment = <<"another">>,
                                                     children = [
                                                                 #rt_node{segment = RegExp,
                                                                          children = [
                                                                                      #rt_node{
                                                                                         segment = <<>>,
                                                                                         value = <<"result">>,
                                                                                         comparator = <<"POST">>
                                                                                        }
                                                                                     ]
                                                                         }
                                                                ]}
                                           ]}
                      ]}],
    ?assertEqual(Expected, Result2).

insert__and_lookup_root_test() ->
    Result = insert('_', <<"/">>, <<"GET">>, <<"Hello world">>, [{'_', []}]),
    {ok, #path_info{value = <<"Hello world">>}} = lookup('_', <<"/">>, <<"GET">>, Result).

insert_and_lookup_binding_test() ->
    Result = insert('_', <<"/:binding">>, <<"GET">>, <<"Hello world">>, [{'_', []}]),
    ?assertEqual({ok, #path_info{value = <<"Hello world">>, bindings = [{<<"binding">>, <<"hi">>}]}}, lookup('_', <<"/hi">>, <<"GET">>, Result)).

insert_and_lookup_binding_and_qs_test() ->
    Result = insert('_', <<"/:binding">>, <<"GET">>, <<"Hello world">>, [{'_', []}]),
    ?assertEqual({ok, #path_info{value = <<"Hello world">>, qs = [{<<"hello">>, <<"world">>}], bindings = [{<<"binding">>, <<"hi">>}]}}, lookup('_', <<"/hi?hello=world">>, <<"GET">>, Result)).


-endif.
