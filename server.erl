-module(server).
-export([start/2]).

start (LMPort, GMPort) -> 
	spawn(fun() -> loginmanager:start(LMPort) end), 
	timer:sleep(200),
	spawn(fun() -> gamemanager:start(GMPort) end).

