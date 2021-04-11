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
  handleCommand(Tokens),
  loop.

% empty commands are ignored
handleCommand([""]).

% quit and end_of_file halt. (note - EOF is a term, but gets coerced into a string)
handleCommand(["quit"]) :-
  writeln("Goodbye!"),
  halt.
handleCommand(["end_of_file"]) :-
  handleCommand(["quit"]).

% add-required <coursename>: adds a course to the required set
handleCommand(["add-required", NameString]) :-
  atom_string(Name, NameString),
  retract(requiredCourses(RequiredCourses)), sort([Name | RequiredCourses], Sorted), assert(requiredCourses(Sorted)).
% list-required: displays the required set
handleCommand(["list-required"]) :-
  requiredCourses(RequiredCourses), writeln(RequiredCourses).
% clear-required: clears the required set
handleCommand(["clear-required"]) :-
  retract(requiredCourses(_)), assert(requiredCourses([])).

% add-optional <coursename>: adds a course to the optional set
handleCommand(["add-optional", NameString]) :-
  atom_string(Name, NameString),
  retract(optionalCourses(OptionalCourses)), sort([Name | OptionalCourses], Sorted), assert(optionalCourses(Sorted)).
% list-optional: displays the optional set
handleCommand(["list-optional"]) :-
  optionalCourses(OptionalCourses), numOptional(NumOptional), write(NumOptional), write(" required from "), writeln(OptionalCourses).
% clear-optional: clears the optional set
handleCommand(["clear-optional"]) :-
  retract(optionalCourses(_)), assert(optionalCourses([])).
% num-optional <n>: sets numOptional - checked for range safety.
handleCommand(["num-optional", NumString]) :-
  optionalCourses(OptionalCourses), length(OptionalCourses, OptionalCoursesLen),
  (
    termString(Num, NumString), Num =< OptionalCoursesLen, Num >= 0, retract(numOptional(_)), assert(numOptional(Num)) ;
    write(NumString), write(" is not a valid number for "), write(OptionalCoursesLen), writeln(" courses")
  ).

% add-constraint no-classes-before <term> <day> <hour> <minute>: adds a constraint to avoid classes between 00:00 and hour:minute on day in term
handleCommand(["add-constraint", "no-classes-before", TermString, DayString, HString, MString]) :-
  (
    termString(Term, TermString), parseDay(DayString, Day), hourString(H, HString), minuteString(M, MString), duration(time(00, 00), time(H, M), Duration),
    retract(constraints(Constraints)), sort([breakTime(interval(Term, Day, time(00, 00), time(H, M)), Duration) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term = "), writeln(TermString),
  write("Day  = "), writeln(DayString),
  write("Hour = "), writeln(HString),
  write("Min  = "), writeln(MString).
% add-constraint no-classes-after <term> <day> <hour> <minute>: adds a constraint to avoid classes between hour:minute and 24:00 on day in term
handleCommand(["add-constraint", "no-classes-after", TermString, DayString, HString, MString]) :-
  (
    termString(Term, TermString), parseDay(DayString, Day), hourString(H, HString), minuteString(M, MString), duration(time(H, M), time(24, 00), Duration),
    retract(constraints(Constraints)), sort([breakTime(interval(Term, Day, time(H, M), time(24, 00)), Duration) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term = "), writeln(TermString),
  write("Day  = "), writeln(DayString),
  write("Hour = "), writeln(HString),
  write("Min  = "), writeln(MString).
% add-constraint break <term> <day> <startHour> <startMinute> <endHour> <endMinute> <hours> <minutes>: adds a constraints to have hours:minutes free between `start` and `end`
handleCommand(["add-constraint", "break", TermString, DayString, StartHString, StartMString, EndHString, EndMString, HString, MString]) :-
  (
    termString(Term, TermString), parseDay(DayString, Day), hourString(StartH, StartHString), minuteString(StartM, StartMString), hourString(EndH, EndHString), minuteString(EndM, EndMString), hourString(H, HString), minuteString(M, MString), duration(time(StartH, StartM), time(EndH, EndM), Duration), before(time(H, M), Duration),
    retract(constraints(Constraints)), sort([breakTime(interval(Term, Day, time(StartH, StartM), time(EndH, EndM)), time(H, M)) | Constraints], Sorted), assert(constraints(Sorted))
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
% add-constraint no-classes-on <term> <day>
handleCommand(["add-constraint", "no-classes-on", TermString, DayString]) :-
  (
    termString(Term, TermString), parseDay(DayString, Day),
    retract(constraints(Constraints)), sort([breakTime(interval(Term, Day, time(0, 0), time(24, 0)), time(24, 0)) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term = "), writeln(TermString),
  write("Day  = "), writeln(DayString).
% add-constraint minimum-credits <term> <credits>
handleCommand(["add-constraint", "minimum-credits", TermString, CreditsString]) :-
  (
    termString(Term, TermString), nonNegativeNumberString(Credits, CreditsString),
    retract(constraints(Constraints)), sort([minimumCredits(Credits, Term) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term    = "), writeln(TermString),
  write("Credits = "), writeln(CreditsString).
% add-constraint maximum-credits <term> <credits>
handleCommand(["add-constraint", "maximum-credits", TermString, CreditsString]) :-
  (
    termString(Term, TermString), nonNegativeNumberString(Credits, CreditsString),
    retract(constraints(Constraints)), sort([maximumCredits(Credits, Term) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term    = "), writeln(TermString),
  write("Credits = "), writeln(CreditsString).
% add-constraint course-in-term <course> <term>
% Doesn't check to make sure course is required or optional - we can have a constraint that just doesn't apply.
handleCommand(["add-constraint", "course-in-term", CourseString, TermString]) :-
  (
    atom_string(Course, CourseString), termString(Term, TermString),
    retract(constraints(Constraints)), sort([courseInTerm(Course, Term) | Constraints], Sorted), assert(constraints(Sorted))
  ) ;
  writeln("Could not parse and validate at least one of:"),
  write("Term   = "), writeln(TermString),
  write("Course = "), writeln(CourseString).
% list-constraints: displays the constraint set
handleCommand(["list-constraints"]) :-
  constraints(Constraints), displayConstraints(Constraints).
% clear-constraints: clear the constraint set
handleCommand(["clear-constraints"]) :-
  retract(constraints(_)), assert(constraints([])).

% schedule: shows all possible schedules using given course lists and constraint set.
handleCommand(["schedule"]) :-
  requiredCourses(RequiredCourses), length(RequiredCourses, RequiredCoursesLen),
  optionalCourses(OptionalCourses), numOptional(NumOptional),
  constraints(Constraints),
  scheduleChoices([courseChoices(RequiredCourses, RequiredCoursesLen), courseChoices(OptionalCourses, NumOptional)], Constraints, Schedules),
  displaySchedules(Schedules).

% Bad command gets caught here
handleCommand(Unrecognized) :-
  write("Invalid command: "), writeln(Unrecognized).

% string <-> non-negative number
nonNegativeNumberString(N, S) :-
  number_string(N, S), 0 =< N.

% string <-> term
termString(T, S) :-
  number_string(T, S), 1 =< T, T =< 2.

% string <-> hour
hourString(H, S) :-
  number_string(H, S), 0 =< H, H < 24.

% string <-> minute
minuteString(M, S) :-
  number_string(M, S), 0 =< M, M < 60.

% day as string to day as term
parseDay("Monday", monday).
parseDay("monday", monday).
parseDay("Tuesday", tuesday).
parseDay("tuesday", tuesday).
parseDay("wednesday", wednesday).
parseDay("Wednesday", wednesday).
parseDay("thursday", thursday).
parseDay("Thursday", thursday).
parseDay("friday", friday).
parseDay("Friday", friday).
parseDay("saturday", saturday).
parseDay("Saturday", saturday).
parseDay("sunday", sunday).
parseDay("Sunday", sunday).

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
displayConstraint(breakTime(interval(Term, Day, time(00, 00), time(24, 00)), time(24, 00))) :-
  write("Free on "), write(Day), write(" in term "), writeln(Term).
displayConstraint(breakTime(interval(Term, Day, Start, End), Duration)) :-
  (
    duration(Start, End, Duration), write("Free between ") ;
    write("At least "), writeTime(Duration), write(" free between ")
  ),
  writeTime(Start), write(" and "), writeTime(End), write(" during "), write(Day), write(" in term "), writeln(Term).
displayConstraint(minimumCredits(Credits, Term)) :-
  write("At least "), write(Credits), write(" credits in term "), writeln(Term).
displayConstraint(maximumCredits(Credits, Term)) :-
  write("No more than "), write(Credits), write(" credits in term "), writeln(Term).
displayConstraint(courseInTerm(Course, Term)) :-
  write(Course), write(" must happen in term "), writeln(Term).

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