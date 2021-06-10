-module(gamemanager).
-import(physics,[update_step/3,collision_handler/4,spawnCreatures/3,spawnPosition/3]).
-import(physics,[timenow/0,epsilon/0,minV/0,screenRatio/0,spawnSize/0]).
-import(physics,[minLinear/0,minAng/0,minObstacles/0,maxObstacles/0,minObstacleSize/0,maxObstacleSize/0,gen_obstacles/2]).
-export([start/1]).

timeout() -> 0 .

start (Port) ->
	LMPid = whereis(loginmanager),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	NumObstacles = round(minObstacles() + (maxObstacles() - minObstacles()) * rand:uniform()),
	Obstacles = gen_obstacles([], NumObstacles),
	register(gamemanager, spawn(fun() -> game(LMPid, dict:new(), dict:new(), Obstacles, timenow(), 0) end)),
	spawn(fun() -> acceptor(LMPid, LSock) end),
	receive stop -> ok end.

acceptor (LMPid, LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LMPid, LSock) end),
	io:format("User entered game lobby~n", []),
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
					LMPid ! {{check, User, Pass}, Sock, gamemanager};
				"press" -> 
					[Key] = Args,
					gamemanager ! {press, Key, Sock};
				"release" ->
					[Key] = Args,
          gamemanager ! {release, Key, Sock};
				_ -> gamemanager ! bad_request
			end,
			parse_requests (LMPid, Sock);
		{tcp_closed, _} ->
			gamemanager ! {user_left, Sock},   
			gen_tcp:close(Sock);
		{tcp_error, _, _} ->
			gamemanager ! {user_left, Sock},   
			gen_tcp:closed(Sock)
	end.

game (LMPid, Users, Creatures, Obstacles, Time, SpawnTime) ->
	NewUsers = user_handler(Users, Obstacles),
	NewTime = timenow(),
	TimeStep = (NewTime - Time) / 1000,
	{NewSpawnTime, SCreatures} = spawnCreatures(SpawnTime + TimeStep, Creatures, Obstacles),
	{UpUsers, UpCreatures} = update_step(NewUsers, SCreatures, TimeStep),
	{ColUsers, ColCreatures} = collision_handler(LMPid, UpUsers, UpCreatures, Obstacles),
	updateClient(ColUsers, ColCreatures),
	FPS = 1 / (epsilon() + TimeStep),
	case FPS < 60 of 
		true -> io:format("~p~n",[FPS]);
		_ -> FPS2 = 0
	end,
	game(LMPid, ColUsers, ColCreatures, Obstacles, NewTime, NewSpawnTime).
	
updateClient(Users, Creatures) ->
	NumUsers = integer_to_list(dict:size(Users)),
	NumCreatures = integer_to_list(dict:size(Creatures)),
	Sockets = dict:fetch_keys(Users),
	[gen_tcp:send(S, list_to_binary(NumUsers ++ " " ++ NumCreatures ++ "\n")) || S <- Sockets],
	[sendUser (U, Sockets) || {_, U} <- dict:to_list(Users)],
	[sendCreature(K, C, Sockets) || {K, C} <- dict:to_list(Creatures)].

sendUser (User, Sockets) ->
	{X, Y} = dict:fetch("pos", User),
	{FW,FA,FD} = dict:fetch("fuel", User),
	UD1 = dict:fetch("name", User),
	UD2 = UD1 ++ " " ++ float_to_list(X) ++ " " ++ float_to_list(Y),
	UD3 = UD2 ++ " " ++ float_to_list(dict:fetch("theta", User)), 
	UD4 = UD3 ++ " " ++ float_to_list(dict:fetch("size", User)),
	UD5 = UD4 ++ " " ++ integer_to_list(dict:fetch("score", User)),
	UD6 = UD5 ++ " " ++ float_to_list(FW) ++ " " ++ float_to_list(FA),
	UD7 = UD6 ++ " " ++ float_to_list(FD) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD7)) || S <- Sockets ].

sendCreature(Name, C, Sockets) ->
	{X, Y} = dict:fetch("pos", C),
	Color = dict:fetch("color", C),
	UD1 = Name ++ " " ++ Color ++ " " ++ float_to_list(X) ++ " " ++ float_to_list(Y),
	UD2 = UD1 ++ " " ++ float_to_list(dict:fetch("size", C)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD2)) || S <- Sockets].

sendObstacle(Obstacle, Sock) ->
	{X,Y} = dict:fetch("pos", Obstacle),
	Size = dict:fetch("size", Obstacle),
	Send = float_to_list(X) ++ " " ++ float_to_list(Y) ++ " " ++ float_to_list(Size) ++ "\n",
	gen_tcp:send(Sock, list_to_binary(Send)).

user_handler(Users, Obstacles) ->
	Size = dict:size(Users), 
	receive
		{ok, User, Sock, loginmanager} when Size < 3 ->
			Player = dict:store("name", User, dict:new()),
			Accel = dict:store("a", {0,0} , Player),
			Vel = dict:store("v", {minV(), 0}, Accel),
			Blobs = [U || {_,U} <- dict:to_list(Users)] ++ Obstacles,
			GenPos = spawnPosition({rand:uniform()*screenRatio(),rand:uniform()}, Blobs, Blobs),
			Pos = dict:store("pos", GenPos, Vel),
			Fuel = dict:store("fuel", {1.0,1.0,1.0}, Pos),
			SizeDict = dict:store("size", spawnSize(), Fuel),
			Orientation = dict:store("theta", 2*math:pi()*rand:uniform(), SizeDict),
			NewUser = dict:store("score", 0, Orientation),
			NewUser1 = dict:store("agility", 0, NewUser),
			NewUser2 = dict:store("collision_flag", false, NewUser1),
			Result = dict:store(Sock, NewUser2, Users),
			io:format("USER ADDED~n"),
			gen_tcp:send(Sock, list_to_binary("user added\n")),
			gen_tcp:send(Sock, list_to_binary("obstacles " ++ integer_to_list(length(Obstacles)) ++ "\n")),
			[sendObstacle (O, Sock) || O <- Obstacles];
		{ok, _, Sock, loginmanager} -> 
			gen_tcp:send(Sock, list_to_binary("game full\n")),
			Result = Users;
		{_, Sock, loginmanager} ->
			gen_tcp:send(Sock, list_to_binary("wrong authentication\n")),
			Result = Users;
		{user_left, Sock} ->
			case dict:is_key(Sock, Users) of
				true ->
					io:format("User removed~n"),
					User = dict:fetch(Sock, Users),
					Name = dict:fetch("name", User),
					Result = dict:erase(Sock, Users),
					[gen_tcp:send(S, list_to_binary(Name ++ " left\n")) || {S, _} <- dict:to_list(Users)];
				_ ->
					Result = Users
			end;
		{creature_died, Name} ->
			Result = Users,
			[gen_tcp:send(S, list_to_binary(Name ++ " died\n")) || {S, _} <- dict:to_list(Users)];
		{press, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{minLinear(), Ang}, User), Users);
		{press, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, Ang-minAng()}, User), Users);
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, Ang+minAng()}, User), Users);
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{-minLinear(), Ang}, User), Users);
		{release, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang + minAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result = dict:store(Sock, dict:store("a", {Linear, NewAng}, NewUser), Users);
		{release, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang - minAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result  = dict:store(Sock,dict:store("a", {Linear, NewAng}, NewUser), Users);
		_ -> Result = Users
	after timeout() -> Result = Users 
	end,
	Result .
