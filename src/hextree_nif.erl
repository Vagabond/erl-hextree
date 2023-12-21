-module(hextree_nif).

-export([
    hextree_new/1,
    hexset_new/1,
    hextree_contains/2,
    hexset_contains/2
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

hextree_new(Map) ->
    ?NOT_LOADED.

hexset_new(List) ->
    ?NOT_LOADED.

hextree_contains(Tree, Index) ->
    ?NOT_LOADED.

hexset_contains(Set, Index) ->
    ?NOT_LOADED.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

init() ->
    SoName =
        case code:priv_dir(hextree) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?MODULE]);
                    false ->
                        filename:join([priv, ?MODULE])
                end;
            Dir ->
                filename:join(Dir, ?MODULE)
        end,
    ok = erlang:load_nif(SoName, 0).
