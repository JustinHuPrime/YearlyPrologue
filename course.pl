% time/2 :- A 24-hour timepoint, with some hour and minute
% interval/3 :- An interval on some day, starting at some timepoint and ending a some timepoint

% A section for some course, with some section number, at some abstract intervals
section(cpsc110, 101, [interval(monday, time(08, 00), time(09, 00))]).
section(cpsc110, 102, [interval(monday, time(09, 00), time(10, 00))]).
section(cpsc121, 101, [interval(monday, time(08, 00), time(09, 00))]).
section(cpsc121, 102, [interval(monday, time(09, 00), time(10, 00))]).

% True if first timepoint is before second timepoint
before(time(H1, M1), time(H2, M2)) :- H1 < H2 ; (H1 = H2, M1 =< M2).

% True if given interval doesn't collide with given interval
noCollide4(interval(D1, S1, E1), interval(D2, S2, E2)) :- D1 \= D2 ; before(E1, S2) ; before(E2, S1).

% True if given interval doesn't collide with given intervals
noCollide3(_, []).
noCollide3(T1, [T2 | Ts]) :- noCollide4(T1, T2), noCollide3(T1, Ts).

% True if given interval doesn't collide with given sections
noCollide2(_, []).
noCollide2(T, [section(_, _, Ts) | Ss]) :- noCollide3(T, Ts), noCollide2(T, Ss).

% True if given intervals don't collide with given sections
noCollide1([], _).
noCollide1([T | Ts], Ss) :- noCollide2(T, Ss), noCollide1(Ts, Ss).

% Is the given course list covered by the sections, and are the sections valid?
valid([], []).
valid([C | Cs], [section(C, N, Ts) | Ss]) :- section(C, N, Ts), valid(Cs, Ss), noCollide1(Ts, Ss). % ordering is important here - must compute Ss before requiring that Ts don't collide with them