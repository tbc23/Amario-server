-module(gamemanager).
-import(vectors,[norm/1,dot/2,add/2,toPolar/1,fromPolar/1,mult/2]).
-export([start/1]).

timeout() -> 0 .
spawn_time() -> 5 .
timenow() -> erlang:monotonic_time(millisecond) .
screenRatio() -> 16/9.
creatureV() -> minV() * 2 .
minV() -> 0.1 .
maxV() -> 1 .
maxW() -> 2 * math:pi() / 1 .
minW() -> -maxW() .
getLinear() -> (maxV() - minV())/2 .
getAng() -> (maxW() - minW())/2 .  
minSize () -> 0.025 .
creatureSize () -> minSize() / 2.
maxCreatures() -> 10.
epsilon () -> 0.0000001.

getV (User) -> 
	{V, _} = dict:fetch("v", User),
	Theta = dict:fetch("theta", User),
	fromPolar({V, Theta}).

putV (VC, User) ->
	{V, Theta} = toPolar(VC),
	{_, W} = dict:fetch("v", User),
	NewUser = dict:store("v", {V,W}, User),
	dict:store("theta", Theta, NewUser).

start (Port) ->
	LMPid = whereis(loginmanager),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	register(gamemanager, spawn(fun() -> game(LMPid, dict:new(), dict:new(), dict:new(), timenow(), 0) end)),
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
          gamemanager ! {release, Key, Sock}
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
	NewUsers = user_handler(Users),
	NewTime = timenow(),
	TimeStep = (NewTime - Time) / 1000,
	{NewSpawnTime, SCreatures} = spawnCreatures(SpawnTime + TimeStep, Creatures),
	{UpUsers, UpCreatures} = update_step(NewUsers, SCreatures, TimeStep),
	{ColUsers, ColCreatures} = collision_handler(UpUsers, UpCreatures),
	updateClient(ColUsers, ColCreatures),
	FPS = 1 / (epsilon() + TimeStep),
	case FPS < 60 of 
		true -> io:format("~p~n",[FPS]);
		_ -> FPS2 = 0
	end,
	game(LMPid, ColUsers, ColCreatures, Obstacles, NewTime, NewSpawnTime).

spawnCreatures(SpawnTime, Creatures) ->
	NumCreatures = dict:size(Creatures),
	case (SpawnTime > spawn_time()) and (NumCreatures < maxCreatures()) of
		true -> 
			NewSpawnTime = 0,
			C1 = dict:store("pos", {rand:uniform()*screenRatio(), rand:uniform()}, dict:new()),
			C2 = dict:store("v", {creatureV(), 0}, C1),
			C3 = dict:store("size", creatureSize(), C2),
			C4 = dict:store("theta", 2*math:pi()*rand:uniform(), C3),
			C5 = dict:store("a", {0, 0}, C4),
			case rand:uniform() > 0.5 of
				true -> Key = "green";
				_ -> Key = "red"
			end,
			C6 = dict:store("color", Key, C5),
			NewCreatures = dict:store(integer_to_list(timenow()), C6, Creatures);
		_ -> {NewSpawnTime, NewCreatures} = {SpawnTime, Creatures}
	end,
	{NewSpawnTime, NewCreatures}.
	
updateClient(Users, Creatures) ->
	NumUsers = integer_to_list(dict:size(Users)),
	NumCreatures = integer_to_list(dict:size(Creatures)),
	Sockets = dict:fetch_keys(Users),
	[gen_tcp:send(S, list_to_binary(NumUsers ++ " " ++ NumCreatures ++ "\n")) || S <- Sockets],
	[sendUser (U, Sockets) || {_, U} <- dict:to_list(Users)],
	[sendCreature(K, C, Sockets) || {K, C} <- dict:to_list(Creatures)].

sendUser (User, Sockets) ->
	{X, Y} = dict:fetch("pos", User),
	UD1 = dict:fetch("name", User),
	UD2 = UD1 ++ " " ++ float_to_list(X) ++ " " ++ float_to_list(Y),
	UD3 = UD2 ++ " " ++ float_to_list(dict:fetch("theta", User)), 
	UD4 = UD3 ++ " " ++ float_to_list(dict:fetch("size", User)),
	UD5 = UD4 ++ " " ++ integer_to_list(dict:fetch("score", User)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD5)) || S <- Sockets ].

sendCreature(Name, C, Sockets) ->
	{X, Y} = dict:fetch("pos", C),
	Color = dict:fetch("color", C),
	UD1 = Name ++ " " ++ Color ++ " " ++ float_to_list(X) ++ " " ++ float_to_list(Y),
	UD2 = UD1 ++ " " ++ float_to_list(dict:fetch("size", C)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD2)) || S <- Sockets].

collision_handler(Users, Creatures) ->
	NUsers = dict:from_list([{K, wall_collision(U)}|| {K, U} <- dict:to_list(Users)]),
	NCreatures = [{K, wall_collision(C)} || {K, C} <- dict:to_list(Creatures)],
	NewCreatures = creature_collision (NCreatures, dict:from_list(NCreatures)),
	{NUsers, NewCreatures}.

creature_collision ([], Creatures) -> Creatures;
creature_collision ([C | Cs], Creatures) ->
	NewCreatures = creature2creature (C, Cs, Creatures),
	creature_collision (Cs, NewCreatures).
	
creature2creature (_, [], CreatureDict) -> CreatureDict;
creature2creature ({K1,C1}, [{K2,C2} | Creatures], CreatureDict) ->
	X1X2 = add(dict:fetch("pos", C1), mult(dict:fetch("pos", C2), -1)), 
	Norm = norm(X1X2),
	case Norm < 2 * creatureSize() of
		true ->
			X2X1 = mult(X1X2, -1),
			{V1,V2} = {getV(C1), getV(C2)}, 
			V1V2 = add(V1, mult(V2, -1)),
			V2V1 = mult(V1V2, -1),
			%io:format("V1V2:~p | V2V1:~p | X1X2:~p | Norm:~p~n", [V1V2,V2V1,X1X2,Norm]),
			Dot = dot(V2V1, mult(X1X2, 1 / (Norm*Norm + epsilon()))),
			VF1 = add(V1, mult(X1X2, Dot)),
			VF2 = add(V2, mult(X2X1, Dot)), 
			NCreatureDict = dict:store(K1, putV(VF1, C1), CreatureDict),
			NewCreatureDict = dict:store(K2, putV(VF2, C2), NCreatureDict);
		_ -> NewCreatureDict = CreatureDict
	end,
	creature2creature ({K1,C1}, Creatures, NewCreatureDict).

wall_collision (User) ->
	{X, Y} = dict:fetch("pos", User),
	Size = dict:fetch("size", User),
	Theta = dict:fetch("theta", User),
	COS = math:cos(Theta),
	SIN = math:sin(Theta),
	CL = (X - Size < 0) and (COS < 0),
	CR = (X + Size > screenRatio()) and (COS > 0),
	CD = (Y + Size > 1) and (SIN > 0),
	CU = (Y - Size < 0) and (SIN < 0),
	NewTheta =
		if 
			(CU or CD) -> - Theta;
			(CL or CR) -> math:pi() - Theta;
		   	true -> Theta	
		end,
	NewUser = dict:store("theta", NewTheta, User),
	NewUser.

update_step(Users, Creatures, Time) ->
	UpUsers = dict:from_list([{K, updateUser(U, Time)} || {K, U} <- dict:to_list(Users)]),
	UpCreatures = dict:from_list([{K, updateCreature(C, Time)} || {K, C} <- dict:to_list(Creatures)]),
	{UpUsers, UpCreatures}.

updateCreature (C, Time) -> updateUser(C, Time). 

updateUser(User, Time) ->
	{Linear, Ang} = dict:fetch("a", User),
	{V, W} = dict:fetch("v", User),
	{X, Y} = dict:fetch("pos", User),
	Theta = dict:fetch("theta", User),
	NewV = V + Linear * Time,
	NewW = W + Ang * Time,
	{Type, UpV} = threshold(NewV, minV(), maxV()),
	{_, UpW} = threshold(NewW, minW(), maxW()),
	case Type of 
		min -> Up = dict:store("a", {0, Ang}, User);
		_ -> Up = User
	end,
	NewT = Theta + W * Time,
	NewX = X + V * math:cos(Theta) * Time,
	NewY = Y + V * math:sin(Theta) * Time,
	Up1 = dict:store("v", {UpV,UpW}, Up),
	Up2 = dict:store("pos", {NewX, NewY}, Up1),
	Up3 = dict:store("theta", NewT, Up2),
	Up3 .

threshold(Value, Min, Max) ->
		if 
			Value < Min -> {min, Min};
			Value > Max -> {max, Max};
			true -> {none, Value}
		end.

user_handler(Users) ->
	Size = dict:size(Users), 
	receive
		{ok, User, Sock, loginmanager} when Size < 3 ->
			Player = dict:store("name", User, dict:new()),
			Accel = dict:store("a", {0,0} , Player),
			Vel = dict:store("v", {minV(), 0}, Accel),
			Pos = dict:store("pos", {rand:uniform()*screenRatio(),rand:uniform()}, Vel),
			SizeDict = dict:store("size", minSize(), Pos),
			Orientation = dict:store("theta", 2*math:pi()*rand:uniform(), SizeDict),
			NewUser = dict:store("score", 0, Orientation),
			Result = dict:store(Sock, NewUser, Users),
			io:format("USER ADDED~n"),
			gen_tcp:send(Sock, list_to_binary("user added\n"));
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
		{press, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{getLinear(), Ang}, User), Users);
		{press, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, Ang-getAng()}, User), Users);
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, Ang+getAng()}, User), Users);
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{-getLinear(), Ang}, User), Users);
		{release, "a", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang + getAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result = dict:store(Sock, dict:store("a", {Linear, NewAng}, NewUser), Users);
		{release, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, Ang} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewAng = Ang - getAng(),
			NewUser = dict:store("v", {V, 0}, User),
			Result  = dict:store(Sock,dict:store("a", {Linear, NewAng}, NewUser), Users)
	after timeout() -> Result = Users 
	end,
	Result .
