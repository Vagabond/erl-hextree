-module(hexset).

-export([new/1, contains/2, from_geojson/2]).

new(List) ->
    hextree_nif:hexset_new(List).

contains(Set, Cell) ->
    hextree_nif:hexset_contains(Set, Cell).

from_geojson(File, Res) ->
    {ok, Bin} = file:read_file(File),
    %% need to parse the geojson and pull out each polygon and use h3:polyfill for each one
    {ok, JSON} = thoas:decode(Bin),
    <<"FeatureCollection">> = maps:get(<<"type">>, JSON),
    Hexes = lists:map(fun(Feature) ->
                        Geo = maps:get(<<"geometry">>, Feature),
                        Coords = maps:get(<<"coordinates">>, Geo),
                        h3:polyfill(lists:map(fun(C) -> lists:map(fun([A, B]) -> {B, A} end, C) end, Coords), Res)
                end, maps:get(<<"features">>, JSON)),
    new(lists:flatten(Hexes)).
