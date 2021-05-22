-module(server).
-export([start/2]).

start (LMPort, GMPort) -> 
	spawn(fun() -> loginmanager:start(LMPort) end), 
	timer:sleep(5000),
	spawn(fun() -> gamemanager:start(GMPort) end).

