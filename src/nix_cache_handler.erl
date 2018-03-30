-module(nix_cache_handler).
-export([init/2]).

init(Req, DB) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    Resp = handle(Path, Req, DB),
    {ok, Resp, DB}.

priority() ->
    os:getenv(<<"NIX_CACHE_PRIORITY">>, "30").

want_mass_query() ->
    os:getenv(<<"NIX_CACHE_WANT_MASS_QUERY">>, "0").

handle("/", Req, _) ->
    cowboy_req:reply(404, Req);
handle("/nix-cache-info", Req, _) ->
    Body = io_lib:format(<<"StoreDir: ~s~n"
			   "WantMassQuery: ~s~n"
			   "Priority: ~s~n">>,
			 [nix_cache_path:root(), want_mass_query(), priority()]),
    cowboy_req:reply(200, #{}, Body, Req);
handle("/" ++ Object, Req, DB) ->
    [Hash, Ext] = string:tokens(Object, "."),
    case nix_cache_hash:is_valid(Hash) of
	true ->
	    case nix_cache_hash:to_path(Hash, DB) of
		{ok, Path} ->
		    dispatch(Hash, Path, Ext, Req);
		not_found ->
		    cowboy_req:reply(404, Req)
	    end;
	false ->
	    cowboy_req:reply(400, Req)
    end.

key_file() ->
    os:getenv("NIX_CACHE_KEY_FILE").

narinfo(Hash, Info) ->
    #{<<"path">> := Path,
      <<"narHash">> := NarHash,
      <<"narSize">> := NarSize,
      <<"deriver">> := Deriver,
      <<"references">> := References,
      <<"signatures">> := Signatures} = Info,
    StrippedReferences = lists:map(fun(P) -> nix_cache_path:strip_store(P) end, References),
    StrippedDeriver = nix_cache_path:strip_store(Deriver),
    io_lib:format(<<"StorePath: ~s~n"
    		    "URL: ~s.nar~n"
    		    "Compression: none~n"
    		    "NarHash: ~s~n"
    		    "NarSize: ~B~n"
    		    "References: ~s~n"
    		    "Deriver: ~s~n"
    		    "Sig: ~s~n">>,
    		  [Path, Hash, NarHash, NarSize,
                   lists:join(" ", StrippedReferences), StrippedDeriver,
                   lists:join(" ", Signatures)]).

dispatch(_, Path, "nar", Req0) ->
    Port = nix_cache_port:spawn("nix", ["dump-path", Path]),
    Req1 = cowboy_req:stream_reply(200, Req0),
    0 = nix_cache_port:stream(Port, Req1);
dispatch(Hash, Path, "narinfo", Req) ->
    nix_cache_path:sign(Path, key_file()),
    NarInfo = narinfo(Hash, nix_cache_path:info(Path)),
    cowboy_req:reply(200, #{}, NarInfo, Req);
dispatch(_, _, _, Req) ->
    cowboy_req:reply(400, Req).
