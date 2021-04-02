% time/2 :- A 24-hour timepoint, with some hour and minute
% interval/3 :- An interval on some day, starting at some timepoint and ending a some timepoint

:- discontiguous section/3, course/3.

% True if first timepoint is before second timepoint
before(time(H1, M1), time(H2, M2)) :- H1 < H2 ; (H1 = H2, M1 =< M2).

% True if given interval doesn't collide with given interval
noCollide4(interval(D1, S1, E1), interval(D2, S2, E2)) :- D1 \= D2 ; before(E1, S2) ; before(E2, S1).

% True if given interval doesn't collide with given intervals
noCollide3(_, []).
noCollide3(T1, [T2 | Ts]) :- noCollide4(T1, T2), noCollide3(T1, Ts).

% True if given interval doesn't collide with given sections
noCollide2(_, _, []).
noCollide2(T, Term, [S | Ss]) :- section(S, term, Term), section(S, time, Ts), noCollide3(T, Ts), noCollide2(T, Term, Ss).
noCollide2(T, Term, [S | Ss]) :- section(S, term, Term2), Term \= Term2, noCollide2(T, Term, Ss).


% True if given intervals don't collide with given sections
noCollide1([], Term, _).
noCollide1([T | Ts], Term, Ss) :- noCollide2(T, Term, Ss), noCollide1(Ts, Term, Ss).

% Is the given course list covered by the sections, and are the sections valid?
%valid([], []).
%valid([C | Cs], [section(C, N, Ts) | Ss]) :- section(C, N, Ts), valid(Cs, Ss), noCollide1(Ts, Ss). % ordering is important here - must compute Ss before requiring that Ts don't collide with them


schedule([], []).
schedule([C | Cs], append(S, Ss, AllSecs)) :- allRequiredSections(C, S), allForCourse(C, S), allSameTerm(S), schedule(Cs, Ss), noCollidingSections(AllSecs).
% I am aware that I check if the later sections collide a whole bunch, could redo so isn't checking that allSecs don't collide

% Given a list of sections, checks if none collide
noCollidingSections([]).
noCollidingSections([S | Ss]) :- section(S, time, Ts), section(S, term, T), noCollide1(Ts, T, Ss), noCollidingSections(Ss).

% Checks that all sections are in the same term
allSameTerm([]).
allSameTerm([S]).
allSameTerm([S1,S2]) :- section(S1, term, T), section(S2, term, T).
allSameTerm([S1,S2|Ss]) :- section(S1, term, T), section(S2, term, T), allSameTerm([S2 | Ss]).

% Checks that all sections are for the same course
allForCourse(C, []).
allForCourse(C, [S | Ss]) :- section(S, course, C), allForCourse(C, Ss).

allRequiredSections(C, []) :- course(C, requiredSections, []).
allRequiredSections(C, S) :- course(C, requiredSections, Reqs), length(S, N), length(Reqs, N), sectionTypes(S, T), containsAll(T, Reqs).

sectionTypes([], []).
sectionTypes([S | Ss], [T | Ts]) :- section(S, type, T), sectionTypes(Ss, Ts).

% containsAll(L1, L2) checks that L1 contains all elements in L2
containsAll(_, []).
containsAll(L1, [H2 | T2]) :- member(H2, L1), containsAll(L1, T2).

% Some course facts, to map out what this looks like
course(cpsc110, prereqs, []).
course(cpsc110, requiredSections, [lecture, lab]).
course(cpsc110, name, "CPSC 110 Computation, Programs, and Programming").
course(cpsc110, credits, 4).

section(cpsc110101, time, [interval(tuesday, time(12, 30), time(14, 00)) | interval(thursday, time(12, 30), time(14, 00))]).
section(cpsc110101, course, cpsc110).
section(cpsc110101, type, lecture).
section(cpsc110101, term, 1).
section(cpsc110L11, time, [interval(wednesday, time(18, 00), time(21, 00))]).
section(cpsc110L11, course, cpsc110).
section(cpsc110L11, type, lab).
section(cpsc110L11, term, 1).
section(cpsc110L21, time, [interval(friday, time(18, 00), time(21, 00))]).
section(cpsc110L21, course, cpsc110).
section(cpsc110L21, type, lab).
section(cpsc110L21, term, 2).

course(cpsc121, prereqs, []).
course(cpsc121, requiredSections, [lecture, lab]).
course(cpsc121, name, "CPSC 121 Models of Computation").
course(cpsc121, credits, 4).

section(cpsc121101, time, [interval(tuesday, time(12, 30), time(14, 00)) | interval(thursday, time(12, 30), time(14, 00))]).
section(cpsc121101, course, cpsc121).
section(cpsc121101, type, lecture).
section(cpsc121101, term, 2).
section(cpsc121L11, time, [interval(wednesday, time(18, 00), time(21, 00))]).
section(cpsc121L11, course, cpsc121).
section(cpsc121L11, type, lab).
section(cpsc121L11, term, 1).
section(cpsc121L21, time, [interval(friday, time(18, 00), time(21, 00))]).
section(cpsc121L21, course, cpsc121).
section(cpsc121L21, type, lab).
section(cpsc121L21, term, 2).