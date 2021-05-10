-module(chatserver).
-export([start/1, stop/1]).

start (Port) -> spawn(fun() -> lobby(Port) end).
stop (Server) -> Server ! stop.

lobby (Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	Room = spawn(fun() -> room([]) end),
	spawn(fun() -> loginmanager:start() end),
	spawn(fun() -> acceptor(LSock, Room) end),
	receive stop -> ok end.

acceptor (LSock, Room) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock, Room) end),	
	io:format("User entered lobby~n", []),
	auth (Sock, Room).

credentials (Sock) ->
	gen_tcp:send(Sock, list_to_binary("Username:")),
	receive User -> ok end,
	gen_tcp:send(Sock, list_to_binary("Password:")),
	receive Pass -> ok end,
	{User, Pass}.

auth (Sock, Room) ->
	gen_tcp:send(Sock, list_to_binary("Authenticate to the server:\n")),
	receive {tcp, _, Msg} ->
		case binary_to_list(Msg) of
			":login" ++ _ -> 
				io:format("login requested~n", []),
				{User, Pass} = credentials (Sock),
				case loginmanager:login(User, Pass) of
					ok -> 
						Room ! {enter, self()},
						user (Sock, Room, User);
					_ -> 
						gen_tcp:send(Sock, list_to_binary("User does not exist.\n")),
						auth (Sock, Room)
				end;
			":create_account" ++ _ -> 
				{User, Pass} = credentials (Sock),
				loginmanager:create_account(User, Pass),
				auth (Sock, Room);
			":close_account" ++ _ -> 
				{User, Pass} = credentials (Sock),
				loginmanager:close_account(User, Pass),
				auth (Sock, Room);
			":logout" ++ _ -> 
				{User, Pass} = credentials (Sock),
				case loginmanager:logout(User, Pass) of
					ok -> 
						Room ! {leave, self()},
						auth (Sock, Room);
					_ -> auth (Sock, Room)
				end;
			":online" ++ _ -> loginmanager:online();
			_ -> 
				gen_tcp:send(Sock, list_to_binary("Invalid option. Try again:\n")),
				auth(Sock, Room)
		end
	end.

room (Pids) ->
	receive 
		{enter, Pid} ->
			io:format("user entered~n", []),
			room([Pid | Pids]);
		{line, Data, Id} = Msg ->
			io:format("received ~p~n", [Data]),
			[P ! Msg || P <- Pids],
			room(Pids);
		{leave, Pid} ->
			io:format("user left~n", []),
			room(Pids -- [Pid])
	end.

user (Sock, Room, User) ->
	receive 
		{line, Data} -> 
			user(Sock, Room, User);
		{tcp, _, Data} ->
			gen_tcp:send(Sock, User ++ " " ++ Data),
			Room ! {line, Data, self()},
			user(Sock, Room, User);
		{tcp_closed, _} ->
			Room ! {leave, self()};
		{tcp_error, _, _} ->
			Room ! {leave, self()}
	end.
