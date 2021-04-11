:- [course].

:- dynamic requiredCourses/1, optionalCourses/1, numOptional/1, constraints/1.
requiredCourses([]). % set of required courses
optionalCourses([]). % set of optional courses
numOptional(0). % how many optional courses to pick - in range [0..length of optionalCourses]
constraints([]). % constraint set

% main command loop
loop :- !,
  read_line_to_string(user_input, Input),
  split_string(Input, " ", " ", Tokens),
  handle_command(Tokens),
  loop.

% empty commands are ignored
handle_command([""]).

% quit and end_of_file halt. (note - EOF is a term, but gets coerced into a string)
handle_command(["quit"]) :-
  writeln("Goodbye!"),
  halt.
handle_command(["end_of_file"]) :-
  handle_command(["quit"]).

% add-required <coursename>: adds a course to the required set
handle_command(["add-required", NameString]) :-
  atom_string(Name, NameString),
  requiredCourses(RequiredCourses),
  retract(requiredCourses(_)), sort([Name | RequiredCourses], Sorted), assert(requiredCourses(Sorted)).
% list-required: displays the required set
handle_command(["list-required"]) :-
  requiredCourses(RequiredCourses), writeln(RequiredCourses).
% clear-required: clears the required set
handle_command(["clear-required"]) :-
  retract(requiredCourses(_)), assert(requiredCourses([])).

% add-optional <coursename>: adds a course to the optional set
handle_command(["add-optional", NameString]) :-
  atom_string(Name, NameString),
  optionalCourses(OptionalCourses),
  retract(optionalCourses(_)), sort([Name | OptionalCourses], Sorted), assert(optionalCourses(Sorted)).
% list-optional: displays the optional set
handle_command(["list-optional"]) :-
  optionalCourses(OptionalCourses), numOptional(NumOptional), write(NumOptional), write(" required from "), writeln(OptionalCourses).
% clear-optional: clears the optional set
handle_command(["clear-optional"]) :-
  retract(optionalCourses(_)), assert(optionalCourses([])).
% num-optional <n>: sets numOptional - checked for range safety.
handle_command(["num-optional", NumString]) :-
  optionalCourses(OptionalCourses), length(OptionalCourses, OptionalCoursesLen),
  (
    number_string(Num, NumString), Num =< OptionalCoursesLen, Num >= 0, retract(numOptional(_)), assert(numOptional(Num)) ;
    write(NumString), write(" is not a valid number for "), write(OptionalCoursesLen), writeln(" courses")
  ).

% add-constraint no-classes-before <term> <day> <hour> <minute>: adds a constraint to avoid classes between 00:00 and hour:minute on day in term
handle_command(["add-constraint", "no-classes-before", TermString, DayString, HString, MString]) :-
  (
    number_string(Term, TermString), parse_day(DayString, Day), hour_string(H, HString), minute_string(M, MString), duration(time(00, 00), time(H, M), Duration),
    constraints(Constraints), retract(constraints(_)), sort([breakTime(interval(Term, Day, time(00, 00), time(H, M)), Duration) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term = "), writeln(TermString),
  write("Day  = "), writeln(DayString),
  write("Hour = "), writeln(HString),
  write("Min  = "), writeln(MString).
% add-constraint no-classes-after <term> <day> <hour> <minute>: adds a constraint to avoid classes between hour:minute and 24:00 on day in term
handle_command(["add-constraint", "no-classes-after", TermString, DayString, HString, MString]) :-
  (
    number_string(Term, TermString), parse_day(DayString, Day), hour_string(H, HString), minute_string(M, MString), duration(time(H, M), time(24, 00), Duration),
    constraints(Constraints), retract(constraints(_)), sort([breakTime(interval(Term, Day, time(H, M), time(24, 00)), Duration) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term = "), writeln(TermString),
  write("Day  = "), writeln(DayString),
  write("Hour = "), writeln(HString),
  write("Min  = "), writeln(MString).
% add-constraint break <term> <day> <startHour> <startMinute> <endHour> <endMinute> <hours> <minutes>: adds a constraints to have hours:minutes free between `start` and `end`
handle_command(["add-constraint", "break", TermString, DayString, StartHString, StartMString, EndHString, EndMString, HString, MString]) :-
  (
    number_string(Term, TermString), parse_day(DayString, Day), hour_string(StartH, StartHString), minute_string(StartM, StartMString), hour_string(EndH, EndHString), minute_string(EndM, EndMString), hour_string(H, HString), minute_string(M, MString), duration(time(StartH, StartM), time(EndH, EndM), Duration), before(time(H, M), Duration),
    constraints(Constraints), retract(constraints(_)), sort([breakTime(interval(Term, Day, time(StartH, StartM), time(EndH, EndM)), time(H, M)) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term       = "), writeln(TermString),
  write("Day        = "), writeln(DayString),
  write("Start Hour = "), writeln(StartHString),
  write("Start Min  = "), writeln(StartMString),
  write("End Hour   = "), writeln(EndHString),
  write("End Min    = "), writeln(EndMString),
  write("Hours      = "), writeln(HString),
  write("Minutes    = "), writeln(MString).
% list-constraints: displays the constraint set
handle_command(["list-constraints"]) :-
  constraints(Constraints), displayConstraints(Constraints).
% clear-constraints: clear the constraint set
handle_command(["clear-constraints"]) :-
  retract(constraints(_)), assert(constraints([])).

% schedule: shows all possible schedules using given course lists and constraint set.
handle_command(["schedule"]) :-
  requiredCourses(RequiredCourses), length(RequiredCourses, RequiredCoursesLen),
  optionalCourses(OptionalCourses), numOptional(NumOptional),
  constraints(Constraints),
  scheduleChoices([courseChoices(RequiredCourses, RequiredCoursesLen), courseChoices(OptionalCourses, NumOptional)], Constraints, Schedules),
  displaySchedules(Schedules).

% Bad command gets caught here
handle_command(Unrecognized) :-
  write("Invalid command: "), writeln(Unrecognized).

% string <-> hour
hour_string(H, S) :-
  number_string(H, S), H < 24.

% string <-> minute
minute_string(M, S) :-
  number_string(M, S), M < 60.

% day as string to day as term
parse_day("Monday", monday).
parse_day("monday", monday).
parse_day("Tuesday", tuesday).
parse_day("tuesday", tuesday).
parse_day("wednesday", wednesday).
parse_day("Wednesday", wednesday).
parse_day("thursday", thursday).
parse_day("Thursday", thursday).
parse_day("friday", friday).
parse_day("Friday", friday).
parse_day("saturday", saturday).
parse_day("Saturday", saturday).
parse_day("sunday", sunday).
parse_day("Sunday", sunday).

% displays a list of constraints with auxiliary length info
displayConstraints([]) :-
  writeln("No constraints").
displayConstraints([C]) :-
  displayConstraintList([C]).
displayConstraints(Cs) :-
  length(Cs, Len),
  write(Len), writeln(" constraints:"),
  displayConstraintList(Cs).

% just displays a list of constraints
displayConstraintList([]).
displayConstraintList([C | Cs]) :-
  displayConstraint(C), displayConstraintList(Cs).

% display a constraint
displayConstraint(breakTime(interval(Term, Day, Start, End), Duration)) :-
  (
    duration(Start, End, Duration), write("Free between ") ;
    write("At least "), writeTime(Duration), write(" free between ")
  ),
  writeTime(Start), write(" and "), writeTime(End), write(" during "), write(Day), write(" in term "), writeln(Term).

% display a time
writeTime(time(H, M)) :-
  write(H), write(":"),
  (
    M < 10, write("0"), write(M) ;
    write(M)
  ).

% display a list of schedules, with auxiliary length info
displaySchedules([]) :-
  writeln("No possible schedules").
displaySchedules([S]) :-
  displayScheduleList([S]).
displaySchedules(Ss) :-
  length(Ss, Len),
  write(Len), writeln(" possible schedules:"),
  displayScheduleList(Ss).

% just display a list of schedules
displayScheduleList([]).
displayScheduleList([S | Ss]) :-
  displaySchedule(S), displayScheduleList(Ss).

% display a schedule
displaySchedule(S) :-
  writeln("--------------------------------------------------------------------------------"),
  displaySectionList(S),
  writeln("--------------------------------------------------------------------------------").

% display a list of sections
displaySectionList([]).
displaySectionList([S | Ss]) :-
  displaySection(S), displaySectionList(Ss).

% display a section
displaySection(S) :-
  section(S, course, C), section(S, type, T), course(C, name, CName), section(S, number, N),
  write(CName), write(" "), write(T), write(" section "), writeln(N).

:- loop.