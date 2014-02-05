-module(store_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EMPTY_STORE, []).
-define(SMALL_STORE, store:put(foo, baa, ?EMPTY_STORE)).
-define(LARGE_STORE, store:put(few, bar, ?SMALL_STORE)).

store_self_test() ->
    ?assertEqual(ok, store:init()),
    ?assertEqual(?EMPTY_STORE, store:new()),
    ?assertEqual(ok, store:destroy(?EMPTY_STORE)).

put_test() ->
    %% add tuple to empty store
    ?assertEqual([{foo,baa}],
        store:put(foo, baa, ?EMPTY_STORE)),
    %% rewrite tuple in a store
    ?assertEqual([{foo,bar}],
        store:put(foo, bar, ?SMALL_STORE)),
    %% add a different tuple in a store
    ?assertEqual([{foo,baa},{few,bar}],
        store:put(few, bar, ?LARGE_STORE)).

delete_test() ->
    %% deleting from an empty store gives an empty store back, not exception
    ?assertEqual(?EMPTY_STORE,
        store:delete(foo, ?EMPTY_STORE)),
    %% deleting last key gives an empty store
    ?assertEqual(?EMPTY_STORE,
        store:delete(foo, ?SMALL_STORE)),
    %% deleting a key gives a small store
    ?assertEqual(?SMALL_STORE,
        store:delete(few, ?LARGE_STORE)).

get_test() ->
    %% getting a key from a store
    ?assertEqual(bar,
        store:get(few, ?LARGE_STORE)),
    %% getting a non-existent item (e.g. from an empty store) returns exception
    ?assertEqual({error, not_found},
        store:get(foo, ?EMPTY_STORE)).

search_test() ->
    %% getting a key from a store
    ?assertEqual([few],
        store:search(bar, ?LARGE_STORE)),
    %% getting a non-existent item (e.g. from an empty store) returns empty list
    ?assertEqual([],
        store:search(foo, ?EMPTY_STORE)).
