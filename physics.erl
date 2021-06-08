-module(physics).
-import(vectors,[norm/1,dot/2,add/2,toPolar/1,fromPolar/1,mult/2]).
-export([update_step/3,collision_handler/3,spawnCreatures/3]).
-export([timenow/0,epsilon/0,minV/0,screenRatio/0,spawnSize/0]).
-export([minLinear/0,minAng/0,gen_obstacles/2,minObstacles/0,maxObstacles/0]).
-export([spawnPosition/3]).

spawn_time() -> 5.
timenow() -> erlang:monotonic_time(millisecond) .
screenRatio() -> 16/9.
creatureV() -> maxV(minSize()) * 1.25.
minV() -> 1/10 . % 10s to fo from bottom to top of screen
maxV(Size) -> minSize() / (2 * Size) . % 1.5s to go from bottom to top of screen if minSize
maxW() -> 2 * math:pi() / 1 .
minW() -> -maxW() .
minLinear() -> (maxV(minSize()) - minV()) / 2 . % 2s to go from min speed to max speed
maxLinear() -> minLinear() * 2. % 1s to do the same as above line
linDecrease() -> (maxLinear() - minLinear()) / 80.
minAng() -> maxW() / 3. % 2s to go from no speed to max angular speed
maxAng() -> minAng() * 2.
minSize() -> 0.025 .
maxSize() -> 3 * minSize().
sizeDecrease() -> (maxSize() - minSize()) / 120.
spawnSize() -> 1.5 * minSize().
creatureSize() -> minSize() / 2.
maxCreatures() -> 10.
maxAgilityPoints() -> 5.
fuelBurnW() -> 1 / (maxLinear() * 4). % 4s to burn at max acceleration 
fuelBurnA() -> 1 / (maxAng() * 3).
fuelRefill() -> fuelBurnW() * maxLinear() / 4. % 4 times longer to refill than to burn
epsilon() -> 0.0000001.
maxCreatureAng() -> 3 * maxAng().
creatureTurningAng() -> 2 * (round(rand:uniform()) - 0.5) * (maxCreatureAng() / 2 * rand:uniform() + maxCreatureAng() / 2).
creatureTurningTime() -> abs(math:pi() / maxW() + math:pi() / (2 * maxW()) * rand:normal()).
creatureWaitTurn() -> abs(1 * rand:normal() + 1.5).
minObstacles() -> 3.
maxObstacles() -> 8.
minObstacleSize() -> 1.5 * minSize().
maxObstacleSize() -> 3 * minObstacleSize().

update_step(Users, Creatures, Time) ->
	UpUsers = dict:from_list([{K, updateUser(U, Time)} || {K, U} <- dict:to_list(Users)]),
	UpCreatures = dict:from_list([{K, updateCreature(C, Time)} || {K, C} <- dict:to_list(Creatures)]),
	{UpUsers, UpCreatures}.

updateCreature (C, Time) -> 
	{Linear, A} = dict:fetch("a", C),
	{V, W} = dict:fetch("v", C),
	{X, Y} = dict:fetch("pos", C),
	Theta = dict:fetch("theta", C),
	{Timer, WaitTime, TurningTime} = dict:fetch("timers", C),
	TimeElapsed = (timenow() - Timer) / 1000,
	NewWaitTime = 
		if 
			(A =/= 0) and (TimeElapsed > TurningTime) -> creatureWaitTurn();
			true -> WaitTime
		end,
	NewTurningTime = 
		if
			(A == 0) and (TimeElapsed > WaitTime) -> creatureTurningTime();
			true -> TurningTime
		end,
	NewTimer = 
		if 
			(A == 0) and (TimeElapsed > WaitTime) -> timenow();
			(A =/= 0) and (TimeElapsed > TurningTime) -> timenow();
			true -> Timer
		end,
	Ang = 
		if 
			(A == 0) and (TimeElapsed > WaitTime) -> creatureTurningAng();
			(A =/= 0) and (TimeElapsed > TurningTime) -> 0;
			true -> A
		end,
	NewV = V + Linear * Time,
	NewW = 
		if 
			(A =/= 0) and (TimeElapsed < TurningTime) -> W + A * Time;
			true -> 0
		end,
	{Type, UpV} = threshold(NewV, minV(), maxV(dict:fetch("size", C))),
	{_, UpW} = threshold(NewW, minW(), maxW()),
	case Type of 
		min -> 
			{_, NAng} = dict:fetch("a", C),
			Up = dict:store("a", {0, NAng}, C);
		_ -> Up = C
	end,
	NewT = Theta + W * Time,
	NewX = X + V * math:cos(Theta) * Time,
	NewY = Y + V * math:sin(Theta) * Time,
	Up1 = dict:store("v", {UpV,UpW}, Up),
	Up2 = dict:store("pos", {NewX, NewY}, Up1),
	Up3 = dict:store("theta", NewT, Up2),
	Up4 = dict:store("a", {Linear, Ang}, Up3),
	Up5 = dict:store("timers", {NewTimer,NewWaitTime,NewTurningTime}, Up4),
	Up5.

updateUser(User, Time) ->
	{L, A} = getAgility(User), 
	{V, W} = dict:fetch("v", User),
	{X, Y} = dict:fetch("pos", User),
	Size = dict:fetch("size", User),
	Theta = dict:fetch("theta", User),
	{FW,FA,FD} = dict:fetch("fuel", User),
	case L > 0 of
		true -> BurnL = 1;
		_ -> BurnL = 0
	end,
	case A < 0 of 
		true -> BurnA = 1; 
		_ -> BurnA = 0
	end,
	{_, NewFW} = threshold(FW + (fuelRefill() - BurnL * L * fuelBurnW()) * Time, 0.0, 1.0),
	{_, NewFA} = threshold(FA + (fuelRefill() + BurnA * A * fuelBurnA()) * Time, 0.0, 1.0),
	{_, NewFD} = threshold(FD + (fuelRefill() - (1-BurnA) * A * fuelBurnA()) * Time, 0.0, 1.0),
	Ang = 
		if 
			(NewFA == 0) and (A < 0) -> 0;
			(NewFD == 0) and (A > 0) -> 0;
			true -> A 
		end,
	Linear = 
		if 
			(NewFW == 0) and (L > 0) -> 0;
			true -> L
		end,
	{NL, NAng} = dict:fetch("a", User),
	case NL > 0 of
		true -> {_, NewL} = threshold(NL - linDecrease() * Time, minLinear(), maxLinear());
		_ -> {_, NewL} = threshold(NL + linDecrease() * Time, -maxLinear(), -minLinear())
	end,
	NewV = V + Linear * Time,
	NewW = W + Ang * Time,
	{Type, UpV} = threshold(NewV, minV(), maxV(Size)),
	{_, UpW} = threshold(NewW, minW(), maxW()),
	case Type of 
		min -> 
			Up = dict:store("a", {0, NAng}, User);
		_ -> Up = User
	end,
	NewT = Theta + W * Time,
	NewX = X + V * math:cos(Theta) * Time,
	NewY = Y + V * math:sin(Theta) * Time,
	{_, NewSize} = threshold(Size - sizeDecrease() * Time, minSize(), maxSize()),
	Up1 = dict:store("v", {UpV,UpW}, Up),
	Up2 = dict:store("pos", {NewX, NewY}, Up1),
	Up3 = dict:store("theta", NewT, Up2),
	Up4 = dict:store("fuel", {NewFW,NewFA,NewFD}, Up3),
	Up5 = dict:store("size", NewSize, Up4),
	Up6 = dict:store("a", {NewL, NAng}, Up5),
	Up6 .

collision_handler(Users, Creatures, Obstacles) ->
	NUsers = [{K, wall_collision(U)}|| {K, U} <- dict:to_list(Users)],
	NCreatures = [{K, wall_collision(C)} || {K, C} <- dict:to_list(Creatures)],
	NCreatures1 = creature_collision (NCreatures, dict:from_list(NCreatures)),
	{NUsers2, NCreatures2} = user_creature_collisions (NUsers, dict:to_list(NCreatures1), dict:from_list(NUsers), NCreatures1),
	NewUsers = obstacle_collisions (dict:to_list(NUsers2), Obstacles, Obstacles, [], false),
	NewCreatures = obstacle_collisions (dict:to_list(NCreatures2), Obstacles, Obstacles, [], false),
	{NewUsers, NewCreatures}.

obstacle_collisions([], _, _, Users, _) -> dict:from_list(Users);
obstacle_collisions([{K,U} | Us], [], Obstacles, Users, Flag) -> 
	NewU = dict:store("collision_flag", Flag, U),
	obstacle_collisions (Us, Obstacles, Obstacles, [{K, NewU} | Users], false);
obstacle_collisions([{K,U} | Us], [O | Os], Obstacles, Users, Flag) ->
	ColFlag = dict:fetch("collision_flag", U),
	X1X2 = add(dict:fetch("pos", U), mult(dict:fetch("pos", O), -1)), 
	{USize, OSize} = {dict:fetch("size", U), dict:fetch("size", O)},
	Norm = norm(X1X2),
	case (Norm < (USize + OSize)) and (ColFlag == false) of
		true ->
			V = mult(getV(U), -1), 
			Dot = dot(V, mult(X1X2, 1 / (Norm*Norm + epsilon()))),
			VF = add(V, mult(X1X2, Dot)), 
			{PV, _} = dict:fetch("v", U),
			NewU = putV(VF, U, PV),
			NewFlag = true;
		_ -> 
			NewFlag = (Norm < (USize + OSize)),
			NewU = U
	end,
	obstacle_collisions([{K,NewU} | Us], Os, Obstacles, Users, (NewFlag) or (Flag)).

user_creature_collisions ([], _, Users, Creatures) -> {Users, Creatures};
user_creature_collisions ([{K,U} | Us], [], Users, Creatures) -> user_creature_collisions (Us, dict:to_list(Creatures), dict:store(K, U, Users), Creatures);
user_creature_collisions ([{KU,U} | Us], [{KC,C} | Cs], Users, Creatures) ->
	XU = dict:fetch("pos", U),
	XC = dict:fetch("pos", C),
	XUXC = add(XU, mult(XC, -1)),
	Size = dict:fetch("size", U),
	case norm(XUXC) < (creatureSize() + Size) of
		true -> 
			Color = dict:fetch("color", C),
			Points = dict:fetch("agility", U),
			UArea = Size * Size * math:pi(),
			CArea = creatureSize() * creatureSize() * math:pi(),
			case Color of
				"green" -> 
					{_, NewPoints} = threshold(Points + 1, 0, maxAgilityPoints()),
					{_, NewSize} = threshold(math:sqrt((UArea + CArea) / math:pi()), minSize(), maxSize());
				_ -> 
					{_, NewPoints} = threshold(Points - 2, 0, maxAgilityPoints()),
					NewSize = Size
			end,
			NUser = dict:store("size", NewSize, U),
			NewUser = dict:store("agility", NewPoints, NUser),
			gamemanager ! {creature_died, KC},
			NewCreatures = dict:erase(KC, Creatures);
		_ -> {NewUser, NewCreatures} = {U, Creatures}
	end,
	user_creature_collisions ([{KU,NewUser} | Us], Cs, Users, NewCreatures).

creature_collision ([], Creatures) -> Creatures;
creature_collision ([C | Cs], Creatures) ->
	NewCreatures = creature2creature (C, Cs, Creatures),
	creature_collision (Cs, NewCreatures).
	
creature2creature (_, [], CreatureDict) -> CreatureDict;
creature2creature ({K1,C1}, [{K2,C2} | Creatures], CreatureDict) ->
	X1X2 = add(dict:fetch("pos", C1), mult(dict:fetch("pos", C2), -1)), 
	Norm = norm(X1X2),
	case Norm < 2 * creatureSize() of
		true ->
			X2X1 = mult(X1X2, -1),
			{V1,V2} = {getV(C1), getV(C2)}, 
			V1V2 = add(V1, mult(V2, -1)),
			V2V1 = mult(V1V2, -1),
			Dot = dot(V2V1, mult(X1X2, 1 / (Norm*Norm + epsilon()))),
			VF1 = add(V1, mult(X1X2, Dot)),
			VF2 = add(V2, mult(X2X1, Dot)), 
			{{PV1,_}, {PV2,_}} = {dict:fetch("v", C1), dict:fetch("v", C2)},
			NCreatureDict = dict:store(K1, putV(VF1, C1, PV1), CreatureDict),
			NewCreatureDict = dict:store(K2, putV(VF2, C2, PV2), NCreatureDict);
		_ -> NewCreatureDict = CreatureDict
	end,
	creature2creature ({K1,C1}, Creatures, NewCreatureDict).

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

getV (User) -> 
	{V, _} = dict:fetch("v", User),
	Theta = dict:fetch("theta", User),
	fromPolar({V, Theta}).

putV (VC, User, PV) ->
	{_, Theta} = toPolar(VC),
	{_, W} = dict:fetch("v", User),
	NewUser = dict:store("v", {PV,W}, User),
	dict:store("theta", Theta, NewUser).

getAgility (User) ->
	Points = dict:fetch("agility", User),
	{Linear, Ang} = dict:fetch("a", User),
	LinearGain = 1 + math:exp(1) / (math:exp(1)-1) * (-1 + maxLinear() / minLinear()) * (1 - math:exp(-Points/maxAgilityPoints())), 
	AngGain = 1 + math:exp(1) / (math:exp(1)-1) * (-1 + maxAng() / minAng() ) * (1 - math:exp(-Points/maxAgilityPoints())), 
	{LinearGain*Linear, AngGain*Ang}.

spawnPosition(Pos, [], _) -> Pos;
spawnPosition(Pos, [B | Bs], Blobs) -> 
	X1X2 = add(Pos, mult(dict:fetch("pos", B), -1)),
	{Size, SizeU} = {minSize(), dict:fetch("size", B)},
	Norm = norm(X1X2),
	case Norm < (Size + SizeU) of
		true -> Result = spawnPosition ({rand:uniform()*screenRatio(), rand:uniform()}, Blobs, Blobs);
		_ -> Result = spawnPosition(Pos, Bs, Blobs)
	end,
	Result.

spawnCreatures(SpawnTime, Creatures, Obstacles) ->
	NumCreatures = dict:size(Creatures),
	case (SpawnTime > spawn_time()) and (NumCreatures < maxCreatures()) of
		true -> 
			NewSpawnTime = 0,
			Pos = spawnPosition({rand:uniform()*screenRatio(), rand:uniform()}, Obstacles, Obstacles),
			C1 = dict:store("pos", Pos, dict:new()),
			C2 = dict:store("v", {creatureV(), 0}, C1),
			C3 = dict:store("size", creatureSize(), C2),
			C4 = dict:store("theta", 2*math:pi()*rand:uniform(), C3),
			C5 = dict:store("a", {0, 0}, C4),
			C6 = dict:store("timers", {timenow(), creatureWaitTurn(), creatureTurningTime()}, C5),
			C7 = dict:store("collision_flag", false, C6),
			case rand:uniform() > 0.5 of
				true -> Key = "green";
				_ -> Key = "red"
			end,
			C8 = dict:store("color", Key, C7),
			NewCreatures = dict:store(integer_to_list(timenow()), C8, Creatures);
		_ -> {NewSpawnTime, NewCreatures} = {SpawnTime, Creatures}
	end,
	{NewSpawnTime, NewCreatures}.

gen_obstacles(Obstacles, 0) -> Obstacles;
gen_obstacles(Obstacles, Size) ->
	NObstacle = dict:store("pos", {rand:uniform()*screenRatio(), rand:uniform()}, dict:new()),
	NewObstacle = dict:store("size", minObstacleSize() + (maxObstacleSize() - minObstacleSize()) * rand:uniform(), NObstacle),
	gen_obstacles([NewObstacle | Obstacles], Size-1).

threshold(Value, Min, Max) ->
		if 
			Value < Min -> {min, Min};
			Value > Max -> {max, Max};
			true -> {none, Value}
		end.

sign (Value) ->
	if 
		Value < 0 -> -1;
		true -> 1
	end.
