-module(nix_cache_handler).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, #{}) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    {ok, handle(Path, Req), #{}}.

handle("/nix-cache-info", Req) ->
    Body = io_lib:format(<<"StoreDir: ~s~n"
			   "WantMassQuery: ~s~n"
			   "Priority: ~s~n">>,
			 nix_store_nif:get_real_store_dir(),
			 os:getenv(<<"NIX_CACHE_PRIORITY">>, "30"),
			 os:getenv(<<"NIX_CACHE_WANT_MASS_QUERY">>, "0")),
    cowboy_req:reply(200, #{}, Body, Req);
handle("/" ++ Object, Req) ->
    try
	serve(Object, Req)
    catch
	error:Reason ->
	    cowboy_req:reply(404, #{<<"content-type">> => <<"text/x-erlang">>},
			     io_lib:format("~p~n", [{Reason, erlang:get_stacktrace()}]), Req)
    end.

serve(Object, Req) ->
    [Hash, Ext] = string:tokens(Object, "."),
    Path = nix_store_nif:query_path_from_hash_part(Hash),
    PathInfo = nix_store_nif:query_path_info(Path),
    dispatch(PathInfo, Ext, Req).

key() ->
    os:getenv(<<"NIX_CACHE_KEY">>, "nix-cache-test:NghpkUOvmdTsppxtAaB5HSKvi+/uX7/JQ8r7CFXHFwrfaPxdzLCum+ntIfvvmp1Q9aalhj0Uq8U1wFMxY1IwLQ==").

dispatch(#{<<"narSize">> := NarSize, <<"path">> := Path}, "nar", Req0) ->
    Headers = #{<<"content-length">> => integer_to_binary(NarSize),
		<<"content-type">> => <<"application/x-nix-nar">>},
    Port = nix_cache_port:spawn("nix", [<<"dump-path">>, Path]),
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    0 = nix_cache_port:stream(Port, Req1);
dispatch(PathInfo0, "narinfo", Req) ->
    PathInfo1 = nix_store_nif:sign(PathInfo0, key()),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/x-nix-narinfo">>},
		     nix_store_nif:path_info_to_narinfo(PathInfo1), Req).
