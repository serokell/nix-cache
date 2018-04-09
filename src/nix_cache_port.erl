-module(nix_cache_port).
-export([spawn/2, stream/2]).

spawn(Command, Args) ->
    erlang:open_port({spawn_executable, os:find_executable(Command)},
		     [{args, Args}, exit_status]).

stream(Port, Request) ->
    receive
	{Port, {data, Data}} ->
	    cowboy_req:stream_body(Data, nofin, Request),
	    stream(Port, Request);
	{Port, {exit_status, Status}} ->
	    cowboy_req:stream_body(<<>>, fin, Request),
	    Status
    end.
