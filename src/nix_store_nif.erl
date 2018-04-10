-module(nix_store_nif).
-export([get_real_store_dir/0, path_info_narinfo/1, path_info_narsize/1, path_info_path/1, path_info_to_map/1,
	 query_path_from_hash_part/1, query_path_info/1, sign/2]).

-on_load(init/0).

init() ->
    NifName = "nix_store_nif",
    NifPath = filename:join(code:priv_dir(nix_cache), NifName),
    ok = erlang:load_nif(NifPath, 0).

get_real_store_dir() ->
    erlang:nif_error(nif_library_not_loaded).

path_info_narinfo(_) ->
    erlang:nif_error(nif_library_not_loaded).

path_info_narsize(_) ->
    erlang:nif_error(nif_library_not_loaded).

path_info_path(_) ->
    erlang:nif_error(nif_library_not_loaded).

path_info_to_map(_) ->
    erlang:nif_error(nif_library_not_loaded).

query_path_from_hash_part(_) ->
    erlang:nif_error(nif_library_not_loaded).

query_path_info(_) ->
    erlang:nif_error(nif_library_not_loaded).

sign(_, _) ->
    erlang:nif_error(nif_library_not_loaded).
