erlc loginmanager.erl gamemanager.erl server.erl
erl -pa ebin -eval "server:start(80, 81)"
