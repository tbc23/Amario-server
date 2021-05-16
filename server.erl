-module(server).
-export([start/1, stop/1, parse_requests/1]).

start (Port) -> spawn(fun() -> lobby(Port) end).
stop (Server) -> Server ! stop.

lobby (Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	spawn(fun() -> loginmanager:start() end),
	spawn(fun() -> acceptor(LSock) end),
	receive stop -> ok end.

acceptor (LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock) end),	
	io:format("User entered lobby~n", []),
	parse_requests (Sock).

parse_requests (Sock) ->
	gen_tcp:send(Sock, list_to_binary("Waiting for request:\n")),
	receive {tcp, _, BinMsg} ->
		Msg = string:replace(binary_to_list(BinMsg), "\n", ""),
		[Req | Args] = string:split(string:replace(Msg, ":", ""), " ", all),
		case Req of 
			"login" -> 
				loginmanager:login(Sock, Args),
				parse_requests (Sock);
			"logout" -> 
				loginmanager:logout(Sock, Args),
				parse_requests (Sock);
			"create_account" -> 
				loginmanager:create_account(Sock, Args),
				parse_requests (Sock);
			"close_account" -> 
				loginmanager:close_account(Sock, Args),
				parse_requests (Sock);
			"online" -> 
				loginmanager:online(Sock),
				parse_requests (Sock);
			"leaderboard" -> 
				loginmanager:leaderboard (Sock),
				parse_requests (Sock);
			"close" -> gen_tcp:close(Sock);
			_ -> 
				gen_tcp:send(Sock, list_to_binary("Invalid request: " ++ Req ++ "\n")),
				parse_requests (Sock)
		end
	end.
