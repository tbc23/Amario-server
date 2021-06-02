-module(gamemanager).
-export([start/1]).

timeout() -> 0 .
timenow() -> erlang:monotonic_time(millisecond) .
screenRatio() -> 16/9.
minV() -> 0.1 .
maxV() -> 0.3 .
maxW() -> 2 * math:pi() / 1 .
minW() -> -maxW() .
getLinear() -> (maxV() - minV())/2 .
getAng() -> (maxW() - minW())/2 .  
minSize () -> 0.025 .

start (Port) ->
	%SPId = whereis(server),
	LMPid = whereis(loginmanager),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	register(gamemanager, spawn(fun() -> game(LMPid, dict:new(), dict:new(), dict:new(), timenow()) end)),
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

game (LMPid, Users, Creatures, Obstacles, Time) ->
	NewUsers = user_handler(Users),
	NewTime = timenow(),
	TimeStep = (NewTime - Time) / 1000,
	{UpUsers, UpCreatures} = update_step(NewUsers, Creatures, TimeStep),
	{ColUsers, ColCreatures} = collision_handler(UpUsers, UpCreatures),
	updateClient(ColUsers, ColCreatures),
	game(LMPid, ColUsers, ColCreatures, Obstacles, NewTime).
	
updateClient(Users, Creatures) ->
	NumUsers = integer_to_list(dict:size(Users)),
	NumCreatures = integer_to_list(dict:size(Creatures)),
	Sockets = dict:fetch_keys(Users),
	[gen_tcp:send(S, list_to_binary(NumUsers ++ " " ++ NumCreatures ++ "\n")) || S <- Sockets],
	UserData = [U || {_, U} <- dict:to_list(Users)],
	sendUsers (UserData, Sockets),
	[sendCreatures(K, C, Sockets) || {K, C} <- dict:to_list(Creatures)].

sendUsers ([], _) -> done ;
sendUsers ([User | Users], Sockets) ->
	{X, Y} = dict:fetch("pos", User),
	UD1 = dict:fetch("name", User),
	UD2 = UD1 ++ " " ++ float_to_list(X) ++ " " ++ float_to_list(Y),
	UD3 = UD2 ++ " " ++ float_to_list(dict:fetch("theta", User)), 
	UD4 = UD3 ++ " " ++ float_to_list(dict:fetch("size", User)),
	UD5 = UD4 ++ " " ++ integer_to_list(dict:fetch("score", User)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD5)) || S <- Sockets ],
	sendUsers(Users, Sockets) .

sendCreatures(_, [], _) -> done;
sendCreatures(Color, [_ | Creatures], Sockets) ->
	sendCreatures (Color, Creatures, Sockets).

collision_handler(Users, Creatures) ->
	NewUsers = dict:from_list([{K, user_collision(U, Users, Creatures)}|| {K, U} <- dict:to_list(Users)]),
	NewCreatures = dict:from_list([{K, creature_collision(C, Users, Creatures)} || {K, C} <- dict:to_list(Creatures)]),
	{NewUsers, NewCreatures}.

creature_collision (Creature, _, _) -> Creature.
user_collision (User, _, _) -> 
	NewUser = wall_collision (User),
	NewUser.

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
	UpCreatures = dict:from_list([{K, updateCreature(U, Time)} || {K, U} <- dict:to_list(Creatures)]),
	{UpUsers, UpCreatures}.

updateCreature (Creature, _) -> Creature .

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
			Pos = dict:store("pos", {rand:uniform(),rand:uniform()}, Vel),
			SizeDict = dict:store("size", minSize(), Pos),
			Orientation = dict:store("theta", 0, SizeDict),
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
			{Linear, _} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, -getAng()}, User), Users);
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, getAng()}, User), Users);
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{-getLinear(), Ang}, User), Users);
		{release, _, Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			{V, _} = dict:fetch("v", User),
			NewUser = dict:store("v", {V, 0}, User),
			Result  = dict:store(Sock,dict:store("a",{Linear, 0}, NewUser), Users)
	after timeout() -> Result = Users 
	end,
	Result .
