-module(hextree).

-export([
    new/1
]).

new(Map) ->
    hextree_nif:hextree_new(Map).
