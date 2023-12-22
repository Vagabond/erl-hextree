
```erlang
%% Create from a list of hexes with hexset:new/1 or from geojson
1> {ok, Set} = hexset:from_geojson("/home/andrew/Downloads/map(2).geojson", 8).
{ok,#Ref<0.1590364644.1850343430.132981>}

%% membership tests
2> hexset:contains(Set, 16#8a44aeaa46a7fff).
true
%% hexset is very fast
3> timer:tc(fun() -> hexset:contains(Set, 16#8844a1228dfffff) end).
{10,true}
4> timer:tc(fun() -> hexset:contains(Set, 16#8c44e2811452bff) end).
{5,false}

%% serializing to disk
6> hexset:to_disktree(Set, <<"test.dt">>).
ok

%% using a pre-serialized hexset from disk
7> {ok, DT} = disktree:open(<<"test.dt">>).
{ok,#Ref<0.1958384599.2282881025.57434>}
8> disktree:contains(DT, 16#8a44aebb02effff).
true
9> disktree:contains(DT, 16#8a2649542aaffff).
false
10> timer:tc(fun() -> disktree:contains(DT, 16#8a2649542aaffff) end).
{11,false}
11> timer:tc(fun() -> disktree:contains(DT, 16#8a44aebb02effff) end).
{20,true}
```
