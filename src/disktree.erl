-module(disktree).

-export([open/1, contains/2]).

open(Filename) ->
    hextree_nif:disktree_open(Filename).

contains(DiskTree, H3) ->
    hextree_nif:disktree_contains(DiskTree, H3).
