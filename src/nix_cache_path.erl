-module(nix_cache_path).
-export([info/1, root/0, sign/2]).

info(Path) ->
    Port = nix_cache_port:spawn("nix", ["path-info", "--json", Path]),
    {0, JSON} = nix_cache_port:consume(Port),
    {ok, [Info], _Rest} = json:decode(JSON),
    Info.

root() ->
    <<"/nix/store">>.

sign(Path, Key) ->
    Port = nix_cache_port:spawn("nix", ["sign-paths", "-k", Key, Path]),
    {0, _} = nix_cache_port:consume(Port).
