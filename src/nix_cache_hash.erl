-module(nix_cache_hash).
-define(SEC, 1000).
-export([is_valid/1, to_path_db/0, to_path/2, valid_chars/0, valid_length/0]).

valid_chars() ->
    "0123456789abcdfghijklmnpqrsvwxyz".

valid_length() ->
    32.

is_valid(Hash) ->
    lists:all(fun(C) -> lists:member(C, valid_chars()) end, Hash)
	andalso length(Hash) == valid_length().

to_path_db() ->
    to_path_db(30 * ?SEC).

to_path_db(Timeout) ->
    File = filename:join([nix_cache_path:root(), "var", "nix", "db", "db.sqlite"]),
    esqlite3:open(File, Timeout, {readonly}).

to_path(Hash, DB) ->
    Prefix = list_to_binary(filename:join(nix_cache_path:store(), Hash)),
    Query = <<"select path from ValidPaths where path >= ? limit 1">>,
    case esqlite3:q(Query, [Prefix], DB) of
	[{Path}] ->
	    case string:prefix(Path, Prefix) of
		nomatch ->
		    {error, not_found};
		_Suffix ->
		    {ok, Path}
	    end;
	[] ->
	    {error, not_found}
    end.
