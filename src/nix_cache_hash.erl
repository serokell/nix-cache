-module(nix_cache_hash).
-export([is_valid/1, resolve/1]).

is_valid(Hash) ->
    ValidChars = "0123456789abcdfghijklmnpqrsvwxyz",
    ValidLength = 32,
    lists:all(fun(C) -> lists:member(C, ValidChars) end, Hash)
	andalso length(Hash) == ValidLength.

resolve(Hash) ->
    [Path] = filelib:wildcard("/nix/store/" ++ Hash ++ "-*"), Path.
