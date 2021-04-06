% time/2 :- A 24-hour timepoint, with some hour and minute
% interval/4 :- An interval in some term, on some day, starting at some timepoint and ending a some timepoint

:- discontiguous section/3, course/3.

% True if first timepoint is before second timepoint
before(time(H1, M1), time(H2, M2)) :- H1 < H2 ; (H1 = H2, M1 =< M2).

% True if first timepoint is strictly before second timepoint
strictlyBefore(time(H1, M1), time(H2, M2)) :- H1 < H2 ; (H1 = H2, M1 < M2).

% True if given interval doesn't collide with given interval
noCollide4(interval(T1, D1, S1, E1), interval(T2, D2, S2, E2)) :- T1 \= T2 ; D1 \= D2 ; before(E1, S2) ; before(E2, S1).

% True if given interval doesn't collide with given intervals
noCollide3(_, []).
noCollide3(T1, [T2 | Ts]) :- noCollide4(T1, T2), noCollide3(T1, Ts).

% True if given interval doesn't collide with given sections
noCollide2(_, _, []).
noCollide2(T, [S | Ss]) :- section(S, time, Ts), noCollide3(T, Ts), noCollide2(T, Ss).

% True if given intervals don't collide with given sections
noCollide1([], _).
noCollide1([T | Ts], Ss) :- noCollide2(T, Ss), noCollide1(Ts, Ss).

% Given a list of sections, checks if none collide
noCollidingSections([]).
noCollidingSections([S | Ss]) :- section(S, time, Ts), noCollide1(Ts, Ss), noCollidingSections(Ss).

% Produces true if AllSchedules is the set of all schedules that cover the given courses and meet given constraints
scheduleAll(Cs, Constraints, AllSchedules) :- setof(S, scheduleSingle(Cs, Constraints, S), AllSchedules).

% Produces true if AllSecs is a list of sections that cover the given courses, do not collide, and meet given constraints
scheduleSingle([], []).
scheduleSingle(Cs, Constraints, AllSecs) :- getSectionList(Cs, AllSecs), noCollidingSections(AllSecs), meetsConstraints(Constraints, AllSecs).

% true if the list of sections covers all of the given courses
getSectionList([],[]).
getSectionList([C | Cs], Sec) :- getSections(C, S), getSectionList(Cs, Ss), append(S, Ss, Sec).

% gets a list of sections for a given course
getSections(C, S) :- course(C, requiredSections, Reqs), allRequiredSections(C, Reqs, S), allSameTerm(S).

% true if for a given course and required section types, we have a section of that type for that course
allRequiredSections(_, [], []).
allRequiredSections(C, [Req | Reqs], [S | Ss]) :- section(S, course, C), section(S, type, Req), allRequiredSections(C, Reqs, Ss).

% Checks that all sections are in the same terms (if one section is in both terms, so must the other sections)
allSameTerm([]).
allSameTerm([S | Ss]) :- section(S, time, Intervals), termInIntervals(Intervals, Terms), allInTerms(Terms, Ss).

% true if all sections are within the given terms
allInTerms(_, []).
allInTerms(Terms, [Section | Sections]) :- section(Section, time, Intervals), termInIntervals(Intervals, Terms), allInTerms(Terms, Sections).

% true if the list of sections meets all of the given constraints
meetsConstraints([], _).
meetsConstraints([Constraint | Constraints], Sections) :- meetsConstraint(Constraint, Sections), meetsConstraints(Constraints, Sections).

meetsConstraint(_, []).

% true if the given sections meet the given constraint
meetsConstraint(prequesitesMet, Sections) :- checkPreReqs(Sections, Sections).
meetsConstraint(breakTime(Interval, Duration), Sections) :- permuteIntervals(Interval, Duration, Intervals), oneIntervalFree(Intervals, Sections).
meetsConstraint(termRequired(Course, Term), Sections) :- inTerm(Course, Term, Sections).

% true if all sections for given course occur in the given term.
inTerm(_, _, []).
inTerm(Course, Term, [Section | Sections]) :- section(Section, course, Course), section(Section, time, Intervals), termInIntervals(Intervals, [Term]), inTerm(Course, Term, Sections).
inTerm(Course, Term, [Section | Sections]) :- section(Section, course, Course2), Course \= Course2, inTerm(Course, Term, Sections).

% true if intervals is all intervals in half hour permutations between the start and end times that last duration long.
permuteIntervals(interval(_, _, Start, End), Duration, []) :- duration(Start, NewEnd, Duration), strictlyBefore(End, NewEnd).
permuteIntervals(interval(Term, Day, Start, End), Duration, [interval(Term, Day, Start, NewEnd) | Intervals]) :- duration(Start, NewEnd, Duration), duration(Start, NewStart, time(0, 30)), permuteIntervals(interval(Term, Day, NewStart, End), Duration, Intervals).

% true if the duration between start and end time is Duration
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), time(DurationHour, DurationMinute)) :- StartMinute =< EndMinute, DurationHour is EndHour - StartHour, DurationMinute is EndMinute - StartMinute.
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), time(DurationHour, DurationMinute)) :- StartMinute > EndMinute, DurationHour is EndHour - StartHour - 1, DurationMinute is EndMinute - StartMinute + 60.

% true if one interval in given intervals doesn't collide with given sections. (ormap)
oneIntervalFree([Interval | _], Sections) :- noCollide2(Interval, Sections).
oneIntervalFree([_ | Intervals], Sections) :- oneIntervalFree(Intervals, Sections).

% true if all prereqs in AllSecs are scheduled before the course they are a prereq for
checkPreReqs([], _).
checkPreReqs([Section | Sections], AllSecs) :- checkPrereq(Section, AllSecs), checkPreReqs(Sections, AllSecs).

% true if the prereqs of the given section happen before that section
checkPrereq(Section, _) :- section(Section, course, Course), course(Course, prereqs, []).
checkPrereq(Section, AllSecs) :- section(Section, course, Course), course(Course, prereqs, PreReqs), section(Section, time, Intervals), terms(Intervals, Terms), allPreReqsBefore(Terms, PreReqs, AllSecs).

% true if all prerequisites occur before the given term.
allPreReqsBefore(_, _, []).
allPreReqsBefore([Term], PreReqs, [Section | Sections]) :- section(Section, course, Course), member(Course, PreReqs), section(Section, time, Intervals), terms(Intervals, [Term2]), Term > Term2, allPreReqsBefore([Term], PreReqs, Sections).
allPreReqsBefore(Terms, PreReqs, [Section | Sections]) :- section(Section, course, Course), nonmember(Course, PreReqs), allPreReqsBefore(Terms, PreReqs, Sections).
% currently, we are assuming a two term system. If there are only two terms and the course occurs in both, it is impossible for the prereqs to occur before the course

% true of terms are the set of terms within the intervals
terms(Intervals, Terms) :- setof(T, termInIntervals(Intervals, T), Terms).

% produce true if given term is within given intervals
termInIntervals([interval(Term, _, _, _) | _], Term).
termInIntervals([_ | Intervals], Term) :- termInIntervals(Intervals, Term).

% helpers
% true if given element isn't in given list
nonmember(_, []).
nonmember(Elem, [Elem | _]) :- !, fail.
nonmember(Elem, [_ | Tail]) :- !, nonmember(Elem, Tail).


% Some course facts, to map out what this looks like
course(cpsc110, prereqs, []).
course(cpsc110, requiredSections, [lecture, lab]).
course(cpsc110, name, "CPSC 110 Computation, Programs, and Programming").
course(cpsc110, credits, 4).

section(cpsc110101, time, [interval(1, tuesday, time(12, 30), time(14, 00)), interval(1, thursday, time(12, 30), time(14, 00))]).
section(cpsc110101, course, cpsc110).
section(cpsc110101, type, lecture).
section(cpsc110101, term, 1).
section(cpsc110L11, time, [interval(1, wednesday, time(18, 00), time(21, 00))]).
section(cpsc110L11, course, cpsc110).
section(cpsc110L11, type, lab).
section(cpsc110L11, term, 1).
section(cpsc110L21, time, [interval(2, friday, time(18, 00), time(21, 00))]).
section(cpsc110L21, course, cpsc110).
section(cpsc110L21, type, lab).
section(cpsc110L21, term, 2).

course(cpsc121, prereqs, []).
course(cpsc121, requiredSections, [lecture, lab]).
course(cpsc121, name, "CPSC 121 Models of Computation").
course(cpsc121, credits, 4).

section(cpsc121101, time, [interval(2, tuesday, time(12, 30), time(14, 00)), interval(2, thursday, time(12, 30), time(14, 00))]).
section(cpsc121101, course, cpsc121).
section(cpsc121101, type, lecture).
section(cpsc121101, term, 2).
section(cpsc121L11, time, [interval(1, wednesday, time(18, 00), time(21, 00))]).
section(cpsc121L11, course, cpsc121).
section(cpsc121L11, type, lab).
section(cpsc121L11, term, 1).
section(cpsc121L21, time, [interval(2, friday, time(18, 00), time(21, 00))]).
section(cpsc121L21, course, cpsc121).
section(cpsc121L21, type, lab).
section(cpsc121L21, term, 2).

course(cpsc100, prereqs, []).
course(cpsc100, requiredSections, [lecture]).
course(cpsc100, credits, 3).

section(cpsc100101, time, [interval(1, tuesday, time(12, 30), time(14, 00)), interval(1, thursday, time(12, 30), time(14, 00))]).
section(cpsc100101, course, cpsc100).
section(cpsc100101, type, lecture).
section(cpsc100101, term, 1).
section(cpsc100102, time, [interval(1, monday, time(12, 30), time(14, 00)), interval(1, friday, time(12, 30), time(14, 00))]).
section(cpsc100102, course, cpsc100).
section(cpsc100102, type, lecture).
section(cpsc100102, term, 1).
section(cpsc100103, time, [interval(1, saturday, time(12, 30), time(14, 00))]).
section(cpsc100103, course, cpsc100).
section(cpsc100103, type, lecture).
section(cpsc100103, term, 1).

course(cpsc200, prereqs, []).
course(cpsc200, requiredSections, [lecture]).
course(cpsc200, credits, 3).

section(cpsc200101, time, [interval(1, tuesday, time(12, 30), time(14, 00)), interval(1, thursday, time(12, 30), time(14, 00))]).
section(cpsc200101, course, cpsc100).
section(cpsc200101, type, lecture).
section(cpsc200101, term, 1).