-module(gamemanager).
-export([start/1]).

start (Port) ->
	LMPid = whereis(loginmanager),
	io:format("Module: ~p~n", [LMPid]),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	register(?MODULE, spawn(fun() -> game(LMPid, dict:new(), dict:new(), dict:new()) end)),
	spawn(fun() -> acceptor(LMPid, LSock) end).

acceptor (LMPid, LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LMPid, LSock) end),
	io:format("User entered game~n", []),
	parse_requests (LMPid, Sock).

parse_requests (LMPid, Sock) ->
	receive 
		{tcp, _, BinMsg} ->
			MsgAux = string:replace(binary_to_list(BinMsg), "\n", ""),
			Msg = string:replace(MsgAux, "\r", ""),
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

game (LMPid, Users, Creatures, Obstacles) ->
	add_user(LMPid, Users, Creatures, Obstacles).


add_user(LMPid, Users, Creatures, Obstacles) ->
	Size = dict:size(Users), 
	receive
		{ok, User, loginmanager} when Size < 3 ->
			Player = dict:store("name", User, dict:new()),
			Accel = dict:store("a", {0,0} , Player),
			Vel = dict:store("v", {0.2, 0.2}, Accel),
			Pos = dict:store("pos", {rand:uniform(),rand:uniform()}, Vel),
			Size = dict:store("size", 0.1, Pos),
			Score = dict:store("score", 0, Size),
			game(LMPid, dict:store(dict:size(Users),Score, Users), Creatures, Obstacles);
		{ok, _, loginmanager} ->
			LMPid ! {game_full, loginmanager};
		_ -> game_full 
	end.	
