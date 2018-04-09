-module(nix_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, Port} = application:get_env(nix_cache, port),
    {ok, _Pid} = cowboy:start_clear(nix_cache, [{port, Port}], #{middlewares => [nix_cache_handler]}),
    nix_cache_sup:start_link().

stop(_State) ->
    ok.
