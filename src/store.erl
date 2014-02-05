-module(store).
-export([init/0,
         new/0,
         put/3,
         delete/2,
         get/2,
         search/2,
         destroy/1]).
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init/0
%% takes no parameters
%% returns ok or {error, Reason} if unable to initialise
init()
    -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% new/0
%% takes no parameters
%% returns a new empty Store, or {error, Reason} if not
new()
    -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% destroy/1
%% takes a Store
%% returns ok on successful deleteion, or {error, Reason} if not
destroy(_Store)
    -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% put/3
%% takes a Key, Value, and Store
%% returns an updated Store, or {error, Reason} if not

%% if store is empty, put new tuple
put(Key, Value, []) ->
    [{Key, Value} | []];

%% if Head tuple matches Key, replace tuple
put(Key, Value, [{Key, _Value} | Store]) ->
    [{Key, Value} | Store];

%% if Head tuple doesn't match Key, move to next Key
put(Key, Value, [{Other_Key, Other_Value} | Store]) ->
    [{Other_Key, Other_Value} | put(Key, Value, Store)].

%% else, the store is corrupted

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete/2
%% takes a Key and Store
%% returns Store without Key, _Value, or {error, Reason} if not

%% if Store is empty, we're done anyway
delete(_, []) ->
    [];

%% if Head tuple matches Key, return Tail of store only
delete(Key, [{Key, _} | Store]) ->
    Store;

%% if Head tuple doesn't match Key, move to next Key
delete(Key, [{Other_Key, Other_Value} | Store]) ->
    [{Other_Key, Other_Value} | delete(Key, Store)].

%% else, the store is corrupted

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get/2
%% takes a Key and Store
%% returns Value for that key or {error, Reason} if not
%% if store is empty, we've not found the Key in the store
get(_store, []) ->
    {error, not_found};

%% if Head tuple matches Key, return Value
get(Key, [{Key, Value} | _store]) ->
    Value;

%% if Head tuple doesn't search Key, move to next Key
get(Key, [{_Other_Key, _Other_Value} | Store]) ->
    get(Key, Store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% search/2
%% takes a Value and Store
%% returns a list of Keys that have the given Value, or {error, Reason}
%% if store is empty, no result was found
search(_Value, []) ->
    [];

%% if Head tuple matches Value, push Key and continue
search(Value, [{Key, Value} | Store]) ->
    [Key | search(Value, Store)];

%% if Head tuple doesn't match Key, move to next Key
search(Value, [ _Tuple | Store]) ->
    search(Value, Store).
