-module(nix_cache_handler).
-export([init/2]).

init(Req, Opts) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    Resp = handle(Path, Req),
    {ok, Resp, Opts}.

handle("/nix-cache-info", Req) ->
    Body = io_lib:format(<<"StoreDir: /nix/store~n"
			   "WantMassQuery: 0~n"
			   "Priority: 30~n">>, []),
    cowboy_req:reply(200, #{}, Body, Req);
handle("/" ++ Object, Req) ->
    [Hash, Ext] = string:tokens(Object, "."),
    case nix_cache_hash:is_valid(Hash) of
	true ->
	    dispatch(Hash, Ext, Req);
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

dispatch(Hash, "nar", Req0) ->
    Path = nix_cache_hash:resolve(Hash),
    Port = nix_cache_port:spawn("nix", ["dump-path", Path]),
    Req1 = cowboy_req:stream_reply(200, Req0),
    0 = nix_cache_port:stream(Port, Req1);
dispatch(Hash, "narinfo", Req) ->
    Path = nix_cache_hash:resolve(Hash),
    nix_cache_path:sign(Path, key_file()),
    NarInfo = narinfo(Hash, nix_cache_path:info(Path)),
    cowboy_req:reply(200, #{}, NarInfo, Req);
dispatch(_, _, Req) ->
    cowboy_req:reply(404, Req).
