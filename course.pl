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
noCollide2(_, []).
noCollide2(T, [S | Ss]) :- section(S, times, Ts), noCollide3(T, Ts), noCollide2(T, Ss).

% True if given intervals don't collide with given sections
noCollide1([], _).
noCollide1([T | Ts], Ss) :- noCollide2(T, Ss), noCollide1(Ts, Ss).

% Given a list of sections, checks if none collide
noCollidingSections([]).
noCollidingSections([S | Ss]) :- section(S, times, Ts), noCollide1(Ts, Ss), noCollidingSections(Ss).


scheduleChoices(Choices, Constraints, AllSchedules) :- setof(C, getCourseList(Choices, C), Lists), scheduleEach(Lists, Constraints, AllSchedules).

scheduleEach([], _, []).
scheduleEach([Courses | Lists], Constraints, AllSchedules) :- scheduleAll(Courses, Constraints, Schedules), scheduleEach(Lists, Constraints, RemainingSchedules), append(Schedules, RemainingSchedules, AllSchedules).

% Produces true if AllSchedules is the set of all schedules that cover the given courses and meet given constraints
scheduleAll(Courses, Constraints, AllSchedules) :- setof(S, scheduleSingle(Courses, Constraints, S), AllSchedules).

% Produces true if AllSecs is a list of sections that cover the given courses, do not collide, and meet given constraints
scheduleSingle([], []).
scheduleSingle(Cs, Constraints, AllSecs) :- getSectionList(Cs, AllSecs), noCollidingSections(AllSecs), meetsConstraints(Constraints, AllSecs).

% true if CourseList contains course choices
getCourseList([], []).
getCourseList([courseChoices(Courses, N) | Choices], CourseList) :- getCourses(Courses, N, C), getCourseList(Choices, Cs), append(C, Cs, CourseList).

% true if course list contains exactly N courses from the selection list
getCourses([], 0, []).
getCourses([C | Cs], N, [C | Rest]) :- Sub is N - 1, getCourses(Cs, Sub, Rest).
getCourses([_ | Cs], N, Rest) :- getCourses(Cs, N, Rest).

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
allSameTerm([S | Ss]) :- section(S, times, Intervals), terms(Intervals, Terms), allInTerms(Terms, Ss).

% true if all sections are within the given terms
allInTerms(_, []).
allInTerms(Terms, [Section | Sections]) :- section(Section, times, Intervals), terms(Intervals, Terms), allInTerms(Terms, Sections).

% true if the list of sections meets all of the given constraints
meetsConstraints([], _).
meetsConstraints([Constraint | Constraints], Sections) :- meetsConstraint(Constraint, Sections), meetsConstraints(Constraints, Sections).

meetsConstraint(_, []).

% true if the given sections meet the given constraint
meetsConstraint(breakTime(Interval, Duration), Sections) :- permuteIntervals(Interval, Duration, Intervals), oneIntervalFree(Intervals, Sections).
meetsConstraint(courseInTerm(Course, Term), Sections) :- inTerm(Course, Term, Sections).
meetsConstraint(minimumCredits(Credits, Term), Sections) :- numberOfCredits(Sections, Term, Credit), Credit >= Credits.
meetsConstraint(maximumCredits(Credits, Term), Sections) :- numberOfCredits(Sections, Term, Credit), Credit =< Credits.

% true if credits is the number of credits in the given term.
numberOfCredits([], _, 0).
numberOfCredits([Section | Sections], Term, Credits) :- 
   (countingSection(Section), section(Section, times, Intervals), terms(Intervals, Terms), member(Term, Terms) 
   -> numberOfCredits(Sections, Term, RemainingCredits), section(Section, course, Course), course(Course, credits, CourseCredits), length(Terms, Div), Credits is RemainingCredits + (CourseCredits / Div) 
   ; numberOfCredits(Sections, Term, Credits)).

% true if it is the section we will count for the section's course. There should only be one such section per course
countingSection(Section) :- section(Section, course, Course), course(Course, requiredSections, [Req|_]), section(Section, type, Req).

% true if all sections for given course occur in the given term.
inTerm(_, _, []).
inTerm(Course, Term, [Section | Sections]) :- section(Section, course, Course), section(Section, times, Intervals), terms(Intervals, [Term]), inTerm(Course, Term, Sections).
inTerm(Course, Term, [Section | Sections]) :- section(Section, course, Course2), Course \= Course2, inTerm(Course, Term, Sections).

% true if intervals is all intervals in half hour permutations between the start and end times that last duration long.
permuteIntervals(interval(_, _, Start, End), Duration, []) :- duration(Start, NewEnd, Duration), strictlyBefore(End, NewEnd).
permuteIntervals(interval(Term, Day, Start, End), Duration, [interval(Term, Day, Start, NewEnd) | Intervals]) :- duration(Start, NewEnd, Duration), duration(Start, NewStart, time(0, 30)), permuteIntervals(interval(Term, Day, NewStart, End), Duration, Intervals).

% true if the duration between start and end time is Duration, (works when we know duration but not end time)
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), time(DurationHour, DurationMinute)) :- (DurationMinute + StartMinute < 60)
    -> EndHour is StartHour + DurationHour, EndMinute is DurationMinute + StartMinute
	; EndHour is DurationHour + StartHour + 1, EndMinute is DurationMinute + StartMinute - 60.

% true if one interval in given intervals doesn't collide with given sections. (ormap)
oneIntervalFree([Interval | _], Sections) :- noCollide2(Interval, Sections).
oneIntervalFree([_ | Intervals], Sections) :- oneIntervalFree(Intervals, Sections).

% true of terms are the set of terms within the intervals
terms(Intervals, Terms) :- setof(T, termInIntervals(Intervals, T), Terms).

% produce true if given term is within given intervals
termInIntervals([interval(Term, _, _, _) | _], Term).
termInIntervals([_ | Intervals], Term) :- termInIntervals(Intervals, Term).


% Some course facts, to map out what this looks like
course(cpsc110, requiredSections, [lecture, lab]).
course(cpsc110, name, "CPSC 110 Computation, Programs, and Programming").
course(cpsc110, credits, 4).

section(cpsc110101, times, [interval(1, tuesday, time(12, 30), time(14, 00)), interval(1, thursday, time(12, 30), time(14, 00))]).
section(cpsc110101, course, cpsc110).
section(cpsc110101, type, lecture).
section(cpsc110L11, times, [interval(1, wednesday, time(18, 00), time(21, 00))]).
section(cpsc110L11, course, cpsc110).
section(cpsc110L11, type, lab).
section(cpsc110L21, times, [interval(2, friday, time(18, 00), time(21, 00))]).
section(cpsc110L21, course, cpsc110).
section(cpsc110L21, type, lab).

course(cpsc121, requiredSections, [lecture, lab]).
course(cpsc121, name, "CPSC 121 Models of Computation").
course(cpsc121, credits, 4).

section(cpsc121101, times, [interval(2, tuesday, time(12, 30), time(14, 00)), interval(2, thursday, time(12, 30), time(14, 00))]).
section(cpsc121101, course, cpsc121).
section(cpsc121101, type, lecture).
section(cpsc121L11, times, [interval(1, wednesday, time(18, 00), time(21, 00))]).
section(cpsc121L11, course, cpsc121).
section(cpsc121L11, type, lab).
section(cpsc121L21, times, [interval(2, friday, time(18, 00), time(21, 00))]).
section(cpsc121L21, course, cpsc121).
section(cpsc121L21, type, lab).

course(cpsc100, requiredSections, [lecture]).
course(cpsc100, credits, 3).

section(cpsc100101, times, [interval(1, tuesday, time(12, 30), time(14, 00)), interval(1, thursday, time(12, 30), time(14, 00))]).
section(cpsc100101, course, cpsc100).
section(cpsc100101, type, lecture).
section(cpsc100102, times, [interval(1, monday, time(12, 30), time(14, 00)), interval(1, friday, time(12, 30), time(14, 00))]).
section(cpsc100102, course, cpsc100).
section(cpsc100102, type, lecture).
section(cpsc100103, times, [interval(1, saturday, time(12, 30), time(14, 00))]).
section(cpsc100103, course, cpsc100).
section(cpsc100103, type, lecture).

course(cpsc200, requiredSections, [lecture]).
course(cpsc200, credits, 3).

section(cpsc200101, times, [interval(1, monday, time(12, 30), time(14, 00)), interval(1, friday, time(12, 30), time(14, 00))]).
section(cpsc200101, course, cpsc200).
section(cpsc200101, type, lecture).
