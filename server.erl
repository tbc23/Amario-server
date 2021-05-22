-module(server).
-export([start/2]).

start (LMPort, GMPort) -> 
	spawn(fun() -> loginmanager:start(LMPort) end), 
	spawn(fun() -> gamemanager:start(GMPort) end).

