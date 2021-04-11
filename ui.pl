:- [course].

:- dynamic requiredCourses/1, optionalCourses/1, numOptional/1, constraints/1.
requiredCourses([]).
optionalCourses([]).
numOptional(0).
constraints([]).

loop :- !,
  read_line_to_string(user_input, Input),
  split_string(Input, " ", " ", Tokens),
  handle_command(Tokens),
  loop.

handle_command([""]).

handle_command(["quit"]) :-
  writeln("Goodbye!"),
  halt.
handle_command(["end_of_file"]) :-
  handle_command(["quit"]).

handle_command(["add-required", NameString]) :-
  atom_string(Name, NameString),
  requiredCourses(RequiredCourses),
  (
    member(Name, RequiredCourses) ;
    retract(requiredCourses(_)), assert(requiredCourses([Name | RequiredCourses]))
  ).
handle_command(["list-required"]) :-
  requiredCourses(RequiredCourses), writeln(RequiredCourses).
handle_command(["clear-required"]) :-
  retract(requiredCourses(_)), assert(requiredCourses([])).

handle_command(["add-optional", NameString]) :-
  atom_string(Name, NameString),
  optionalCourses(OptionalCourses),
  (
    member(Name, OptionalCourses) ;
    retract(optionalCourses(_)), assert(optionalCourses([Name | OptionalCourses]))
  ).
handle_command(["list-optional"]) :-
  optionalCourses(OptionalCourses), numOptional(NumOptional), write(NumOptional), write(" required from "), writeln(OptionalCourses).
handle_command(["clear-optional"]) :-
  retract(optionalCourses(_)), assert(optionalCourses([])).
handle_command(["num-optional", NumString]) :- optionalCourses(OptionalCourses), length(OptionalCourses, OptionalCoursesLen),
  (
    number_string(Num, NumString), Num =< OptionalCoursesLen, Num >= 0, retract(numOptional(_)), assert(numOptional(Num)) ;
    write(NumString), write(" is not a valid number for "), write(OptionalCoursesLen), writeln(" courses")
  ). 

handle_command(["schedule"]) :-
  requiredCourses(RequiredCourses), length(RequiredCourses, RequiredCoursesLen),
  optionalCourses(OptionalCourses), numOptional(NumOptional),
  constraints(Constraints),
  scheduleChoices([courseChoices(RequiredCourses, RequiredCoursesLen), courseChoices(OptionalCourses, NumOptional)], Constraints, Schedules),
  displaySchedules(Schedules).

handle_command(Unrecognized) :-
  write("Invalid command: "), writeln(Unrecognized).

displaySchedules([]) :-
  writeln("No possible schedules").
displaySchedules([S]) :-
  displayScheduleList([S]).
displaySchedules(Ss) :-
  length(Ss, Len),
  write(Len), writeln(" possible schedules:"),
  displayScheduleList(Ss).

displayScheduleList([]).
displayScheduleList([S | Ss]) :- displaySchedule(S), displayScheduleList(Ss).

displaySchedule(S) :-
  writeln("--------------------------------------------------------------------------------"),
  displaySectionList(S),
  writeln("--------------------------------------------------------------------------------").

displaySectionList([]).
displaySectionList([S | Ss]) :-
  displaySection(S), displaySectionList(Ss).

displaySection(S) :-
  section(S, course, C), section(S, type, T), course(C, name, CName), section(S, number, N),
  write(CName), write(" "), write(T), write(" section "), writeln(N).

:- loop.