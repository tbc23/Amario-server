-module(loginmanager).
-export([start/1, stop/1, parse_requests/1]).

stop (Server) -> Server ! stop.

start (Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	io:format("LMSocket: ~p~n", [LSock]),
	spawn(fun() -> startlm() end),
	spawn(fun() -> acceptor(LSock) end),
	receive stop -> ok end.

acceptor (LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock) end),	
	io:format("User entered lobby~n", []),
	parse_requests (Sock).

parse_requests (Sock) ->
	receive 
		{tcp, _, BinMsg} ->
			MsgAux = string:replace(binary_to_list(BinMsg), "\n", ""),
			Msg = string:replace(MsgAux, "\r", ""),
			[Req | Args] = string:split(string:replace(Msg, ":", ""), " ", all),
			case Req of 
				"login" -> 
					login(Sock, Args),
					parse_requests (Sock);
				"logout" -> 
					logout(Sock, Args),
					parse_requests (Sock);
				"create_account" -> 
					create_account(Sock, Args),
					parse_requests (Sock);
				"close_account" -> 
					close_account(Sock, Args),
					parse_requests (Sock);
				"online" -> 
					online(Sock),
					parse_requests (Sock);
				"leaderboard" -> 
					leaderboard (Sock, Args),
					parse_requests (Sock);
				"check" ->
					check_user (Sock, Args),
					parse_requests (Sock);
				"close" -> gen_tcp:close(Sock);
				_ -> 
					gen_tcp:send(Sock, list_to_binary("Invalid request: " ++ Req ++ "\n")),
					parse_requests (Sock)
			end;
		{tcp_closed, _} -> gen_tcp:close(Sock);
		{tcp_error, _, _} -> gen_tcp:close(Sock)
	end.

startlm () -> register(loginmanager, spawn(fun() -> loop(dict:new()) end)).

server_call (Sock, Request) ->
	loginmanager ! {Request, self()},
	receive {Res, loginmanager} -> 
		case is_atom(Res) of 
			true -> R = atom_to_list(Res);
			false -> R = Res
		end,
		gen_tcp:send(Sock, list_to_binary(R ++ "\n"))
	end.

create_account (Sock, Args) -> 
	[User, Pass] = Args,
	server_call (Sock, {create_account, User, Pass}).

close_account (Sock, Args) -> 
	[User, Pass] = Args,
	server_call (Sock, {close_account, User, Pass}).

login (Sock, Args) -> 
	[User, Pass] = Args,
	server_call (Sock, {login, User, Pass}).

logout (Sock, Args) -> 
	[User, Pass] = Args,
	io:format("Req: Logout | User: ~p | Pass: ~p~n", [User, Pass]),
	server_call (Sock, {logout, User, Pass}).

leaderboard (Sock, Args) -> 
	[Num] = Args,
	server_call (Sock, {leaderboard, list_to_integer(Num)}).

online (Sock) -> server_call (Sock, online).

check_user (Sock, Args) ->
	[User, Pass] = Args,
	server_call (Sock, {check, User, Pass}).

loop (Users) ->
	receive 
		{{create_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {ok, loginmanager},
					loop (dict:store(User, {Pass, false, 0}, Users));
				_ ->
					From ! {user_exists, loginmanager},
					loop (Users)
			end;
		{{close_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, loginmanager},
					loop (Users);
				{ok, {Pass, _, _}} -> 
					From ! {ok, loginmanager},
					loop (dict:erase(User, Users));
				_ -> 
					From ! {wrong_authentication, loginmanager},
					loop (Users)
			end;
		{{login, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, loginmanager},
					loop (Users);
				{ok, {Pass, false, HScore}} ->
					From ! {ok, loginmanager},
					loop (dict:store(User, {Pass, true, HScore}, Users));
				{ok, {_, true, _}} ->
					From ! {user_already_logged, loginmanager},
					loop (Users);
				_ -> 
					From ! {wrong_authentication, loginmanager},
					loop (Users)
			end;
		{{logout, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, loginmanager},
					loop (Users);
				{ok, {Upass, true, HScore}} ->
					From ! {ok, loginmanager},
					loop (dict:store(User, {Upass, false, HScore}, Users));
				{ok, {_, false, _}} ->
					From ! {user_not_online, loginmanager},
					loop (Users);
				_ -> 
					From ! {wrong_authentication, loginmanager},
					loop (Users)
			end;
		{online, From} ->
			From ! {[User ++ " " || {User, {_, true, _}} <- dict:to_list(Users)], loginmanager},
			loop (Users);
		{{leaderboard, Num}, From} ->
			F = fun({_, A2}, {_, B2}) -> A2 =< B2 end,
			L = [{User, Score} || {User, {_, _, Score}} <- dict:to_list(Users)],
			{Board, _} = lists:split(min(Num, length(L)), lists:sort(F, L)),
			StrBoard = [User ++ " " ++ integer_to_list(Score) ++ "," || {User, Score} <- Board],
			From ! {StrBoard, loginmanager},
			loop (Users);
		{{check, User, Pass}, Sock, gamemanager} ->
			case dict:find(User, Users) of
				error ->
					gamemanager ! {user_not_found, Sock, loginmanager},
					loop(Users);
				{ok, {Pass, _, _}} -> 
					gamemanager ! {ok, User, Sock, loginmanager},
					loop (Users);
				_ -> 
					gamemanager ! {wrong_authentication, Sock, loginmanager},
					loop (Users)
			end;
		{update_score, Name, Score} ->
			{Pass, Flag, HScore} = dict:fetch(Name, Users),
			loop (dict:store(Name, {Pass, Flag, HScore+Score}, Users));
		{stop, From} -> From ! {ok, loginmanager}
	end.
