-module(vectors).
-import(math, [sqrt/1, acos/1, cos/1, sin/1, atan2/2]).
-export([dot/2, add/2, norm/1, angleBetween/2, toPolar/1, fromPolar/1, mult/2]).

dot ({X1,Y1}, {X2,Y2}) -> X1*X2 + Y1*Y2.
add ({X1,Y1}, {X2,Y2}) -> {X1+X2, Y1+Y2}.
norm ({X,Y}) -> sqrt(X*X + Y*Y).
mult ({X,Y},A) -> {A*X,A*Y}.
angleBetween (X1, X2) -> acos(dot(X1, X2) / (norm(X1)*norm(X2))).
fromPolar ({R, Theta}) -> {R*cos(Theta), R*sin(Theta)}. 
toPolar ({X, Y}) -> {norm({X,Y}), atan2(Y,X)}.
