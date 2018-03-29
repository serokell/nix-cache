-module(nix_cache_hash).
-export([is_valid/1, to_path_db/0, to_path/2]).

is_valid(Hash) ->
    ValidChars = "0123456789abcdfghijklmnpqrsvwxyz",
    ValidLength = 32,
    lists:all(fun(C) -> lists:member(C, ValidChars) end, Hash)
	andalso length(Hash) == ValidLength.

-define(TIMEOUT, 30000).

to_path_db() ->
    esqlite3:open("/nix/var/nix/db/db.sqlite", ?TIMEOUT, {readonly}).

to_path_query() ->
    <<"select path from ValidPaths where path >= ? limit 1">>.

to_path(Hash, DB) ->
    case esqlite3:q(to_path_query(), [filename:join(nix_cache_path:root(), Hash)], DB) of
	[{Path}] ->
	    {ok, Path};
	[] ->
	    not_found
    end.
