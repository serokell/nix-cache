-module(nix_store_nif).
-on_load(init/0).

-export([queryPathFromHashPart/1,
         sign/2,
         getUri/0,
         getStorePath/0,
         isValidPath/1,
         pathInfo/1,
         pathInfo2NarInfo/1,
         pathInfo2map/1
        ]).

init() ->
    NifName = "nix_store_nif",
    NifPath = filename:join(code:priv_dir(nix_cache), NifName),
    ok = erlang:load_nif(NifPath, 0).

queryPathFromHashPart(_hashPart) ->
    erlang:nif_error(nif_library_not_loaded).

sign(_storePath, _key) ->
    erlang:nif_error(nif_library_not_loaded).

getUri() ->
    erlang:nif_error(nif_library_not_loaded).

getStorePath() ->
    erlang:nif_error(nif_library_not_loaded).


isValidPath(_storePath) ->
    erlang:nif_error(nif_library_not_loaded).

pathInfo(_storePath) ->
    erlang:nif_error(nif_library_not_loaded).

pathInfo2NarInfo(_path) ->
    erlang:nif_error(nif_library_not_loaded).

pathInfo2map(_path) ->
    erlang:nif_error(nif_library_not_loaded).
