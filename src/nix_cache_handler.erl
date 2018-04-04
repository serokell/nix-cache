-module(nix_cache_handler).
-export([init/2]).

init(Req, DB) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    Resp = handle(Path, Req, DB),
    {ok, Resp, DB}.

handle("/nix-cache-info", Req, _) ->
    Body = nix_cache_narinfo:format(
	     #{<<"StoreDir">> => nix_cache_path:store(),
	       <<"WantMassQuery">> => os:getenv(<<"NIX_CACHE_WANT_MASS_QUERY">>, "0"),
	       <<"Priority">> => os:getenv(<<"NIX_CACHE_PRIORITY">>, "30")}),
    cowboy_req:reply(200, #{}, Body, Req);
handle("/" ++ Object, Req, DB) ->
    case string:tokens(Object, ".") of
	[Hash, Ext] ->
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
	    end;
	_ ->
	    cowboy_req:reply(404, Req)
    end.

key_file() ->
    os:getenv(<<"NIX_CACHE_KEY_FILE">>).

dispatch(_, Path, "nar", Req0) ->
    case nix_cache_path:info(Path) of
	{ok, #{<<"narSize">> := NarSize}} ->
	    Port = nix_cache_port:spawn("nix", ["dump-path", Path]),
	    Req1 = cowboy_req:stream_reply(200, #{<<"Content-Length">> => NarSize,
						  <<"Content-Type">> => <<"application/x-nix-nar">>}, Req0),
	    0 = nix_cache_port:stream(Port, Req1);
	none ->
	    cowboy_req:reply(404, Req)
    end;
dispatch(Hash, Path, "narinfo", Req) ->
    nix_cache_path:sign(Path, key_file()),
    case nix_cache_path:info(Path) of
	{ok, Info} ->
	    #{<<"path">> := Path,
	      <<"narHash">> := NarHash,
	      <<"narSize">> := NarSize,
	      <<"deriver">> := Deriver,
	      <<"references">> := References0,
	      <<"signatures">> := Signatures} = Info,
	    References1 = lists:map(fun(F) -> filename:basename(F) end, References0),
	    Body = nix_cache_narinfo:format(
		     #{<<"StorePath">> => Path,
		       <<"URL">> => Hash ++ ".nar",
		       <<"Compression">> => <<"none">>,
		       <<"NarHash">> => NarHash,
		       <<"NarSize">> => integer_to_binary(NarSize),
		       <<"References">> => nix_cache_narinfo:join(References1),
		       <<"Deriver">> => filename:basename(Deriver),
		       <<"Sig">> => nix_cache_narinfo:join(Signatures)}),
	    cowboy_req:reply(200, #{}, Body, Req);
	none ->
	    cowboy_req:reply(404, Req)
    end;
dispatch(_, _, _, Req) ->
    cowboy_req:reply(400, Req).
