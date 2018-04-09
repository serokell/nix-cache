-module(nix_cache_info).
-export([format/1, join/1]).

format(M) ->
    ToLine = fun(K, V) -> lists:join(<<": ">>, [K, V]) end,
    Lines = maps:values(maps:map(ToLine, M)),
    lists:join(<<"\n">>, Lines ++ [<<>>]).

join(Components) ->
    lists:join(<<" ">>, Components).
