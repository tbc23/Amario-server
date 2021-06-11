-module(gamemanager).
-import(physics,[update_step/3,collision_handler/4,spawnCreatures/3,spawnPosition/3]).
-import(physics,[timenow/0,epsilon/0,minV/0,screenRatio/0,spawnSize/0]).
-import(physics,[minLinear/0,minAng/0,minObstacles/0,maxObstacles/0,minObstacleSize/0,maxObstacleSize/0,gen_obstacles/2]).
-export([start/1]).

timeout() -> 0 .
queueMax() -> 3 .

start (Port) ->
	LMPid = whereis(loginmanager),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	NumObstacles = round(minObstacles() + (maxObstacles() - minObstacles()) * rand:uniform()),
	Obstacles = gen_obstacles([], NumObstacles),
	register(gamemanager, spawn(fun() -> game(LMPid, dict:new(), dict:new(), Obstacles, timenow(), 0, []) end)),
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
			gamemanager ! {user_left, Sock, LMPid, "left"},   
			gen_tcp:close(Sock);
		{tcp_error, _, _} ->
			gamemanager ! {user_left, Sock, LMPid, "left"},   
			gen_tcp:closed(Sock)
	end.

game (LMPid, Users, Creatures, Obstacles, Time, SpawnTime, Queue) ->
	{NewQueue, NewUsers} = user_handler(Users, Obstacles, Queue),
	NewTime = timenow(),
	TimeStep = (NewTime - Time) / 1000,
	{NewSpawnTime, SCreatures} = spawnCreatures(SpawnTime + TimeStep, Creatures, Obstacles),
	{UpUsers, UpCreatures} = update_step(NewUsers, SCreatures, TimeStep),
	{ColUsers, ColCreatures} = collision_handler(LMPid, UpUsers, UpCreatures, Obstacles),
	updateClient(ColUsers, ColCreatures),
	updateQueue(Queue, 1),
	game(LMPid, ColUsers, ColCreatures, Obstacles, NewTime, NewSpawnTime, NewQueue).

updateQueue ([], _) -> ok;
updateQueue ([{Sock,_} | Socks], Counter) ->
	gen_tcp:send(Sock, list_to_binary(integer_to_list(Counter) ++ " ahead\n")),
	updateQueue(Socks, Counter+1).

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
	UD2 = UD1 ++ " " ++ float_to_list(dict:fetch("size", C)),
	UD3 = UD2 ++ " " ++ float_to_list(dict:fetch("theta", C)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD3)) || S <- Sockets].

sendObstacle(Obstacle, Sock) ->
	{X,Y} = dict:fetch("pos", Obstacle),
	Size = dict:fetch("size", Obstacle),
	Send = float_to_list(X) ++ " " ++ float_to_list(Y) ++ " " ++ float_to_list(Size) ++ "\n",
	gen_tcp:send(Sock, list_to_binary(Send)).

createPlayer(User, Users, Obstacles) ->
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
	NewUser2.

eraseQueue (_, QH, []) -> QH;
eraseQueue (Sock, QH, [{S,U} | Qs]) ->
	case Sock == S of
		true -> 
			Result = QH ++ Qs,
			eraseQueue (Sock, Result, []);
		_ -> 
			Result = QH ++ [{S,U}],
			eraseQueue (Sock, Result, Qs)
	end.

user_handler(Users, Obstacles, Queue) ->
	Size = dict:size(Users), 
	QMax = queueMax(),
	receive
		{ok, User, Sock, loginmanager} when Size < 3 ->
			NewUser = createPlayer(User, Users, Obstacles),
			Result = {Queue, dict:store(Sock, NewUser, Users)},
			io:format("USER ADDED~n"),
			gen_tcp:send(Sock, list_to_binary("user added\n")),
			gen_tcp:send(Sock, list_to_binary("obstacles " ++ integer_to_list(length(Obstacles)) ++ "\n")),
			[sendObstacle (O, Sock) || O <- Obstacles];
		{ok, _, Sock, loginmanager} when length(Queue) >= QMax -> 
			gen_tcp:send(Sock, list_to_binary("game full\n")),
			Result = {Queue, Users};
		{ok, User, Sock, loginmanager} ->
			gen_tcp:send(Sock, list_to_binary("added to queue\n")),
			Result = {Queue ++ [{Sock,User}], Users};
		{_, Sock, loginmanager} ->
			gen_tcp:send(Sock, list_to_binary("wrong authentication\n")),
			Result = {Queue, Users};
		{user_left, Sock, LMPid, Option} ->
			case dict:is_key(Sock, Users) of
				true ->
					case length(Queue) > 0 of
						true -> 
							[{HSock, HUser} | NewQueue] = Queue,
							gamemanager ! {ok, HUser, HSock, loginmanager};
						_ -> NewQueue = Queue
					end,
					io:format("User removed~n"),
					User = dict:fetch(Sock, Users),
					Name = dict:fetch("name", User),
					Result = {NewQueue, dict:erase(Sock, Users)},
					[gen_tcp:send(S, list_to_binary(Name ++ " " ++ Option ++ "\n")) || {S, _} <- dict:to_list(Users)],
					LMPid ! {{logout, Name, "pass"}, gamemanager};
				_ ->
					NewQueue = eraseQueue(Sock, [], Queue),
					Result = {NewQueue, Users}
			end;
		{creature_died, Name} ->
			Result = {Queue, Users},
			[gen_tcp:send(S, list_to_binary(Name ++ " died\n")) || {S, _} <- dict:to_list(Users)];
		{press, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = {Queue, dict:store(Sock,dict:store("a",{minLinear(), Ang}, User), Users)};
		{press, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = {Queue, dict:store(Sock,dict:store("a",{Linear, Ang-minAng()}, User), Users)};
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = {Queue, dict:store(Sock,dict:store("a",{Linear, Ang+minAng()}, User), Users)};
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = {Queue, dict:store(Sock,dict:store("a",{-minLinear(), Ang}, User), Users)};
		{release, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang + minAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result = {Queue, dict:store(Sock, dict:store("a", {Linear, NewAng}, NewUser), Users)};
		{release, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang - minAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result  = {Queue, dict:store(Sock,dict:store("a", {Linear, NewAng}, NewUser), Users)};
		_ -> Result = {Queue, Users}
	after timeout() -> Result = {Queue, Users} 
	end,
	Result .
