-module(nix_cache_handler).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, #{}) ->
    {ok, handle(cowboy_req:path(Req), Req), #{}}.

boolean_to_integer(true) -> 1;
boolean_to_integer(false) -> 0.

handle(<<"/nix-cache-info">>, Req) ->
    {ok, WantMassQuery} = application:get_env(nix_cache, want_mass_query),
    {ok, Priority} = application:get_env(nix_cache, priority),
    Body = io_lib:format(<<"StoreDir: ~s~nWantMassQuery: ~B~nPriority: ~B~n">>,
			 [nix_store_nif:get_real_store_dir(), boolean_to_integer(WantMassQuery), Priority]),
    cowboy_req:reply(200, #{}, Body, Req);
handle(<<$/, Object/binary>>, Req) ->
    try
	[Hash, Ext] = string:lexemes(Object, "."),
	Path = nix_store_nif:query_path_from_hash_part(Hash),
	PathInfo = nix_store_nif:query_path_info(Path),
	serve(Path, PathInfo, Ext, Req)
    catch
	error:Reason ->
	    cowboy_req:reply(404, #{<<"content-type">> => <<"text/x-erlang">>},
			     io_lib:format("~p~n", [{Reason, erlang:get_stacktrace()}]), Req)
    end.

serve(Path, PathInfo, <<"nar">>, Req0) ->
    Headers = #{<<"content-length">> => integer_to_binary(nix_store_nif:path_info_narsize(PathInfo)),
		<<"content-type">> => <<"application/x-nix-nar">>},
    Port = nix_cache_port:spawn("nix", [<<"dump-path">>, Path]),
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    0 = nix_cache_port:stream(Port, Req1);
serve(_, PathInfo0, <<"narinfo">>, Req) ->
    {ok, Key} = application:get_env(nix_cache, key),
    PathInfo1 = nix_store_nif:sign(PathInfo0, Key),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/x-nix-narinfo">>},
		     nix_store_nif:path_info_narinfo(PathInfo1), Req).
