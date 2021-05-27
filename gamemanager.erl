-module(gamemanager).
-export([start/1]).

getAng () -> 10 .

getLinear () -> 50 .

start (Port) ->
	SPId = whereis(server),
	LMPid = whereis(loginmanager),
	io:format("Module: ~p~n", [LMPid]),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	io:format("GMSocket: ~p~n", [LSock]),
	register(gamemanager, spawn(fun() -> game(LMPid, dict:new(), dict:new(), dict:new()) end)),
	spawn(fun() -> acceptor(LMPid, LSock) end),
	receive stop -> ok end.

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
					LMPid ! {{check, User, Pass}, Sock, gamemanager},
				"press" -> 
					[Key] = Args,
					gamemanager ! {press, Key, Sock},
				"release" ->
					[Key] = Args,
                    gamemanager ! {release, Key, Sock},
			end,
			parse_requests (LMPid, Sock);
		{tcp_closed, _} ->
			gamemanager ! {user_left, Sock},   
			gen_tcp:close(Sock);
		{tcp_error, _, _} ->
			gamemanager ! {user_left, Sock},   
			gen_tcp:closed(Sock)
	end.

game (LMPid, Users, Creatures, Obstacles, Time) ->
	user_handler(LMPid, Users, Creatures, Obstacles)
	{UpUsers, UpCreatures} = update_step(Users, Creatures)
%	game(LMPid, UpUsers, UpCreatures, Obstacles).
	.

update_step(Users, Creatures, Time) ->
	UpUsers = dict:new(),
	UpCreatures = dict:new(),
	[dict:store(K, updateUser(U, Time), UpUsers) || {K, U} <- dict:to_list(Users)]

updateUser(User, Time) ->
	{Linear, Ang} = dict:fetch("a", User),
	{V, Theta} = dict:fetch("v", User),
	{X, Y} = dict:fetch("pos", User),
	Up1 = dict:store("v", {V + Linear * Time, Theta + 0.5 * Ang * Time * Time}, User),
	Up2 = dict:store("pos", {X + V * Time * math:cos(Theta), Y + V * Time * math:sin(Theta)}, Up1),
	Up2 .

	
user_handler(LMPid, Users, Creatures, Obstacles) ->
	Size = dict:size(Users), 
	receive
		{ok, User, Sock, loginmanager} when Size < 3 ->
			Player = dict:store("name", User, dict:new()),
			Accel = dict:store("a", {0,0} , Player),
			Vel = dict:store("v", {0.2, 0.2}, Accel),
			Pos = dict:store("pos", {rand:uniform(),rand:uniform()}, Vel),
			SizeDict = dict:store("size", 0.1, Pos),
			Score = dict:store("score", 0, SizeDict),
			io:format("USER ADDED~n"),
			gen_tcp:send(Sock, list_to_binary("user added\n")),
			game(LMPid, dict:store(Sock, Score, Users), Creatures, Obstacles);
		{ok, _, Sock, loginmanager} -> 
			gen_tcp:send(Sock, list_to_binary("game full\n")),
			game(LMPid, Users, Creatures, Obstacles);	
		{_, Sock, loginmanager} ->
			gen_tcp:send(Sock, list_to_binary("wrong authentication\n")),
			game(LMPid, Users, Creatures, Obstacles);
		{user_left, Sock} ->
			case dict:is_key(Sock, Users) of
				true ->
					io:format("User removed~n"),
					game(LMPid, dict:erase(Sock, Users), Creatures, Obstacles);
				_ ->
					game(LMPid, Users, Creatures, Obstacles)
			end;
		{press, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			UpUsers = dict:store(Sock,dict:store("a",{getLinear(), Ang}, User), Users),
			game(LMPid, UpUsers, Creatures, Obstacles);
		{press, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			UpUsers = dict:store(Sock,dict:store("a",{Linear, getAng()}, User), Users),
			game(LMPid, UpUsers, Creatures, Obstacles);
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			UpUsers = dict:store(Sock,dict:store("a",{Linear, -getAng()}, User), Users),
			game(LMPid, UpUsers, Creatures, Obstacles);
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			UpUsers = dict:store(Sock,dict:store("a",{-getLinear(), Ang}, User), Users),
			game(LMPid, UpUsers, Creatures, Obstacles);
		{release, _, Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			UpUsers = dict:store(Sock,dict:store("a",{Linear, 0}, User), Users),
			game(LMPid, UpUsers, Creatures, Obstacles)
	end.	
