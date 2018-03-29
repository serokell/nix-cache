-module(nix_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, DB} = nix_cache_hash:to_path_db(),
    Dispatch = cowboy_router:compile([{'_', [{'_', nix_cache_handler, DB}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 17672}], #{env => #{dispatch => Dispatch}}),
    nix_cache_sup:start_link().

stop(_State) ->
    ok.
