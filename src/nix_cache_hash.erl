-module(nix_cache_hash).
-export([is_valid/1, to_path/1]).

is_valid(Hash) ->
    ValidChars = "0123456789abcdfghijklmnpqrsvwxyz",
    ValidLength = 32,
    lists:all(fun(C) -> lists:member(C, ValidChars) end, Hash)
	andalso length(Hash) == ValidLength.

to_path(Hash) ->
    case filelib:wildcard("/nix/store/" ++ Hash ++ "-*") of
	[Path] ->
	    {ok, Path};
	[] ->
	    not_found
    end.
