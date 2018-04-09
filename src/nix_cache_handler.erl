-module(nix_cache_handler).
-export([init/2]).

init(Req, DB) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    {ok, handle(Path, Req, DB), DB}.

handle("/nix-cache-info", Req, _) ->
    Info = #{<<"StoreDir">> => nix_cache_path:store(),
	     <<"WantMassQuery">> => os:getenv(<<"NIX_CACHE_WANT_MASS_QUERY">>, "0"),
	     <<"Priority">> => os:getenv(<<"NIX_CACHE_PRIORITY">>, "30")},
    cowboy_req:reply(200, #{}, nix_cache_info:format(Info), Req);
handle("/" ++ Object, Req, DB) ->
    try
	serve(Object, Req, DB)
    catch
	error:Reason ->
	    cowboy_req:reply(404, #{<<"content-type">> => <<"text/x-erlang">>},
			     io_lib:format("~p~n", [{Reason, erlang:get_stacktrace()}]), Req)
    end.

serve(Object, Req, DB) ->
    [Hash, Ext] = string:tokens(Object, "."),
    true = nix_cache_hash:is_valid(Hash),
    {ok, Path} = nix_cache_hash:to_path(Hash, DB),
    {ok, PathInfo} = nix_cache_path:info(Path),
    dispatch(PathInfo, Ext, Req).

key_file() ->
    os:getenv(<<"NIX_CACHE_KEY_FILE">>, "/run/keys/nix-cache").

narinfo(#{<<"deriver">> := Deriver,
	  <<"narHash">> := NarHash,
	  <<"narSize">> := NarSize,
	  <<"path">> := Path,
	  <<"references">> := References0,
	  <<"signatures">> := Signatures}) ->
    References1 = lists:map(fun(F) -> filename:basename(F) end, References0),
    #{<<"StorePath">> => Path,
      <<"URL">> => nix_cache_path:to_hash(Path) ++ ".nar",
      <<"Compression">> => <<"none">>,
      <<"NarHash">> => NarHash,
      <<"NarSize">> => integer_to_binary(NarSize),
      <<"References">> => nix_cache_info:join(References1),
      <<"Deriver">> => filename:basename(Deriver),
      <<"Sig">> => nix_cache_info:join(Signatures)}.

narinfo_with_defaults(PathInfo) ->
    narinfo(maps:merge(#{<<"deriver">> => <<>>}, PathInfo)).

dispatch(#{<<"narSize">> := NarSize, <<"path">> := Path}, "nar", Req0) ->
    Headers = #{<<"content-length">> => integer_to_binary(NarSize),
		<<"content-type">> => <<"application/x-nix-nar">>},
    Port = nix_cache_port:spawn("nix", [<<"dump-path">>, Path]),
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    0 = nix_cache_port:stream(Port, Req1);
dispatch(#{<<"path">> := Path} = PathInfo, "narinfo", Req) ->
    nix_cache_path:sign(Path, key_file()),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/x-nix-narinfo">>},
		     nix_cache_info:format(narinfo_with_defaults(PathInfo)), Req).
