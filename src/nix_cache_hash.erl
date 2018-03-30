-module(nix_cache_hash).
-export([is_valid/1, to_path_db/0, to_path/2]).
-define(SEC, 1000).

is_valid(Hash) ->
    ValidChars = "0123456789abcdfghijklmnpqrsvwxyz",
    ValidLength = 32,
    lists:all(fun(C) -> lists:member(C, ValidChars) end, Hash)
	andalso length(Hash) == ValidLength.

to_path_db() ->
    to_path_db(30 * ?SEC).

to_path_db(Timeout) ->
    esqlite3:open("/nix/var/nix/db/db.sqlite", Timeout, {readonly}).

to_path(Hash, DB) ->
    Query = <<"select path from ValidPaths where path >= ? limit 1">>,
    case esqlite3:q(Query, [filename:join(nix_cache_path:root(), Hash)], DB) of
	[{Path}] ->
	    {ok, Path};
	[] ->
	    not_found
    end.
