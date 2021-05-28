-module(gamemanager).
-export([start/1]).

timeout() -> 0 .
timenow() -> erlang:monotonic_time(millisecond) .
minV() -> 0.1 .
maxV() -> 0.3 .
minW() -> 0 .
maxW() -> 2 * math:pi() / 1 .
getLinear() -> (maxV() - minV())/2 .
getAng() -> (maxW() - minW())/2 .  

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

%ShowUsers ([]) -> done;
%ShowUsers ([User | Users]) -> 
%	UserList = dict:to_list(User),
%	[io:format("~p: ~p | ", [K, V]) || {K, V} <- UserList],
%	io:format("~n"),
%	ShowUsers(Users).

game (LMPid, Users, Creatures, Obstacles, Time) ->
	NewUsers = user_handler(Users),
	NewTime = timenow(),
	TimeStep = (NewTime - Time) * 1000,
	%Sockets = [S || {S, _} <- dict:to_list(NewUsers)],
	%io:format("Sockets: ~p~n", [Sockets]),
	{UpUsers, UpCreatures} = update_step(NewUsers, Creatures, TimeStep),
	%NewSockets = [S || {S, _} <- dict:to_list(UpUsers)],
	%io:format("New Sockets: ~p~n", [NewSockets]),
	updateClient(UpUsers, UpCreatures),
	game(LMPid, UpUsers, UpCreatures, Obstacles, NewTime).
	
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
	UD3 = UD2 ++ " " ++ float_to_list(dict:fetch("size", User)),
	UD4 = UD3 ++ " " ++ integer_to_list(dict:fetch("score", User)) ++ "\n",
	[gen_tcp:send(S, list_to_binary(UD4)) || S <- Sockets ],
	sendUsers(Users, Sockets) .

sendCreatures(_, [], _) -> done;
sendCreatures(Color, [_ | Creatures], Sockets) ->
	sendCreatures (Color, Creatures, Sockets).

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
			Vel = dict:store("v", {minV(), minW()}, Accel),
			Pos = dict:store("pos", {rand:uniform(),rand:uniform()}, Vel),
			SizeDict = dict:store("size", 0.1, Pos),
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
					Result = dict:erase(Sock, Users);
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
			Result = dict:store(Sock,dict:store("a",{Linear, getAng()}, User), Users);
		{press, "d", Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{Linear, -getAng()}, User), Users);
		{release, "w", Sock} ->
			User = dict:fetch(Sock, Users),
			{_, Ang} = dict:fetch("a", User),
			Result = dict:store(Sock,dict:store("a",{-getLinear(), Ang}, User), Users);
		{release, _, Sock} ->
			User = dict:fetch(Sock, Users),
			{Linear, _} = dict:fetch("a", User),
			Result  = dict:store(Sock,dict:store("a",{Linear, 0}, User), Users)
	after timeout() -> Result = Users 
	end,
	Result .
