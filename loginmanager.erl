-module(loginmanager).
-export([start/1, stop/1, parse_requests/1]).

start (Port) -> spawn(fun() -> lobby(Port) end).
stop (Server) -> Server ! stop.

lobby (Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, true}]),
	spawn(fun() -> startlm() end),
	spawn(fun() -> acceptor(LSock) end),
	receive stop -> ok end.

acceptor (LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock) end),	
	io:format("User entered lobby~n", []),
	parse_requests (Sock).

parse_requests (Sock) ->
	%gen_tcp:send(Sock, list_to_binary("Waiting for request:\n")),
	receive {tcp, _, BinMsg} ->
		Msg = string:replace(binary_to_list(BinMsg), "\n", ""),
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
				leaderboard (Sock),
				parse_requests (Sock);
			"close" -> gen_tcp:close(Sock);
			_ -> 
				gen_tcp:send(Sock, list_to_binary("Invalid request: " ++ Req ++ "\n")),
				parse_requests (Sock)
		end
	end.

startlm () -> register(?MODULE, spawn(fun() -> loop(dict:new()) end)).

server_call (Sock, Request) ->
	?MODULE ! {Request, self()},
	receive {Res, ?MODULE} -> 
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

leaderboard (Sock) -> server_call (Sock, leaderboard).
online (Sock) -> server_call (Sock, online).

loop (Users) ->
	receive 
		{{create_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, false, 0}, Users));
				_ ->
					From ! {user_exists, ?MODULE},
					loop (Users)
			end;
		{{close_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, ?MODULE},
					loop (Users);
				{ok, {Pass, _, _}} -> 
					From ! {ok, ?MODULE},
					loop (dict:erase(User, Users));
				_ -> 
					From ! {wrong_authentication, ?MODULE},
					loop (Users)
			end;
		{{login, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, ?MODULE},
					loop (Users);
				{ok, {Pass, _, HScore}} ->
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, true, HScore}, Users));
				_ -> 
					From ! {wrong_authentication, ?MODULE},
					loop (Users)
			end;
		{{logout, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, ?MODULE},
					loop (Users);
				{ok, {Pass, _, HScore}} ->
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, false, HScore}, Users));
				_ -> 
					From ! {wrong_authentication, ?MODULE},
					loop (Users)
			end;
		{online, From} ->
			From ! {[User ++ " " || {User, {_, true, _}} <- dict:to_list(Users)], ?MODULE},
			loop (Users);
		{leaderboard, From} ->
			F = fun({_, A2}, {_, B2}) -> A2 =< B2 end,
			L = [{User, Score} || {User, {_, _, Score}} <- dict:to_list(Users)],
			{Board, _} = lists:split(min(10, length(L)), lists:sort(F, L)),
			StrBoard = ["(" ++ User ++ "," ++ integer_to_list(Score) ++ ") " || {User, Score} <- Board],
			From ! {StrBoard, ?MODULE},
			loop (Users);
		{stop, From} -> From ! {ok, ?MODULE}
	end.
