-module(loginmanager).
-export([start/0, stop/1, create_account/2, close_account/2, login/2, logout/2, online/1, loop/1]).

start () -> register(?MODULE, spawn(fun() -> loop(dict:new()) end)).

server_call (Sock, Request) ->
	?MODULE ! {Request, self()},
	receive {Res, ?MODULE} -> 
		io:format("Response: ~p~n", [Res]),
		case is_atom(Res) of 
			true -> R = atom_to_list(Res);
			false -> R = Res
		end,
		gen_tcp:send(Sock, list_to_binary(R ++ "\n"))
	end.

create_account (Sock, Args) -> 
	[User, Pass] = Args,
	io:format("Req: Create Account | User: ~p | Pass: ~p~n", [User, Pass]),
	server_call (Sock, {create_account, User, Pass}).

close_account (Sock, Args) -> 
	[User, Pass] = Args,
	io:format("Req: Close Account | User: ~p | Pass: ~p~n", [User, Pass]),
	server_call (Sock, {close_account, User, Pass}).

login (Sock, Args) -> 
	[User, Pass] = Args,
	io:format("Req: Login | User: ~p | Pass: ~p~n", [User, Pass]),
	server_call (Sock, {login, User, Pass}).

logout (Sock, Args) -> 
	[User, Pass] = Args,
	io:format("Req: Logout | User: ~p | Pass: ~p~n", [User, Pass]),
	server_call (Sock, {logout, User, Pass}).

online (Sock) -> server_call (Sock, online).
stop (Sock) -> server_call (Sock, stop).

loop (Users) ->
	receive 
		{{create_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, false}, Users));
				_ ->
					From ! {user_exists, ?MODULE},
					loop (Users)
			end;
		{{close_account, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, ?MODULE},
					loop (Users);
				{ok, {Pass, _}} -> 
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
				{ok, {Pass, _}} ->
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, true}, Users));
				_ -> 
					From ! {wrong_authentication, ?MODULE},
					loop (Users)
			end;
		{{logout, User, Pass}, From} ->
			case dict:find(User, Users) of
				error -> 
					From ! {user_not_found, ?MODULE},
					loop (Users);
				{ok, {Pass, _}} ->
					From ! {ok, ?MODULE},
					loop (dict:store(User, {Pass, false}, Users));
				_ -> 
					From ! {wrong_authentication, ?MODULE},
					loop (Users)
			end;
		{online, From} ->
			From ! {[User || {User, {_, true}} <- dict:to_list(Users)], ?MODULE},
			loop (Users);
		{stop, From} -> From ! {ok, ?MODULE}
	end.
