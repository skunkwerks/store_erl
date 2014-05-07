-module(gen_store).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
    {init,    0},
    {new,     0},
    {put,     3},
    {delete,  2},
    {get,     2},
    {search,  2},
    {destroy, 1}];
behaviour_info(_) -> undefined.