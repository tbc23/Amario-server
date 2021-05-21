-module(gamemanager).
-export([start/1]).

start (Port) ->
	LMPid = whereis(loginmanager),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	% LMPid ! {{check, "user", "user"}, self()},
	register(?MODULE, spawn(fun() -> game(dict:new()) end)),
	spawn(fun() -> acceptor(LMPid, LSock) end).

acceptor (LMPid, LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock) end),
	io:format("User entered game~n", []),
	parse_requests (LMPid, Sock).

parse_requests (LMPid, Sock) ->
	receive 
		{tcp, _, BinMsg} ->
			Msg = string:replace(binary_to_list(BinMsg), "\n", ""),
			[Req | Args] = string:split(string:replace(Msg, ":", ""), " ", all),
			case Req of
				"check" ->
					[User, Pass] = Args,
					LMPid ! {{check, User, Pass}, ?MODULE},
					parse_requests (LMPid, Sock)
			end;
		{tcp_closed, _} -> gen_tcp:close(Sock);
		{tcp_error, _, _} -> gen_tcp:closed(Sock)
	end.

%game (Users, Creatures, Obstacles) -> 
%	receive 

