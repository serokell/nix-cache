-module(nix_cache_path).
-export([info/1, root/0, sign/2, store/0, to_hash/1]).

info(Path) ->
    Port = nix_cache_port:spawn("nix", [<<"path-info">>, <<"--json">>, Path]),
    {0, JSON} = nix_cache_port:consume(Port),
    case json:decode(JSON) of
	{ok, [Info], _Rest} -> {ok, Info};
	{error, Reason} -> {error, Reason}
    end.

root() ->
    os:getenv(<<"NIX_CACHE_ROOT">>, "/nix").

sign(Path, Key) ->
    nix_cache_port:consume(nix_cache_port:spawn("nix", [<<"sign-paths">>, <<"-k">>, Key, Path])).

store() ->
    filename:join(root(), "store").

to_hash(Path) when is_binary(Path) ->
    to_hash(binary_to_list(Path));
to_hash(Path) ->
    lists:sublist(filename:basename(Path), nix_cache_hash:valid_length()).
