-module(nix_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

port() ->
    list_to_integer(os:getenv(<<"NIX_CACHE_PORT">>, "17672")).

start(_Type, _Args) ->
    {ok, _} = cowboy:start_clear(nix_cache_http, [{port, port()}], #{middlewares => [nix_cache_handler]}),
    nix_cache_sup:start_link().

stop(_State) ->
    ok.
