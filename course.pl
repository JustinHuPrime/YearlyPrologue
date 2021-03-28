/**
 * weeklyTime(dow, h, m): a weekly 24-hour time point (dow is one of sunday, monday, ..., friday, saturday)
 */
weeklyTime(D, H, M) :- 0 =< H, H < 24, 0 =< M, M < 60, member(D, [sunday, monday, tuesday, wednesday, thursday, friday, saturday]).

/**
 * interval(start, end): a time interval - start and end are times which must have the same day of the week
 */
interval(weeklyTime(D, H1, M1), weeklyTime(D, H2, M2)) :- weeklyTime(D, H1, M1), weeklyTime(D, H2, M2).

/**
 * section(course, number, intervals): the named course has a section (with the
 * given number) that meets during the given intervals
 * 
 * intervals must not be empty
 */
section(cpsc110, 101, [
  interval(weeklyTime(monday, 08, 00), weeklyTime(monday, 09, 00))
  ]).

/**
 * noCollide(a, b): true if intervals a and b don't collide
 */
noCollide(interval(weeklyTime(D1, SH1, SM1),
                   weeklyTime(D1, EH1, EM1)),
          interval(weeklyTime(D2, SH2, SM2),
                   weeklyTime(D2, EH2, EM2))) :-
  D1 \= D2 ;
  after(SH1, SM1, EH2, EM2) ;
  after(SH2, SM2, EH1, EM1).

/**
 * after(h1, m1, h2, m2): true if first time is after second time
 */
after(H1, _, H2, _) :- H2 < H1.
after(H, M1, H, M2) :- M2 < M1.

/**
 * noCollides(i, l): true if no interval in l collides with i
 */
noCollides(_, []).
noCollides(I1, [I2 | I2s]) :- noCollide(I1, I2), noCollides(I1, I2s).

/**
 * noCollisions(a, b): true if no interval in a collides with any interval in b
 */
noCollisions([], _).
noCollisions([A | As], B) :- noCollides(A, B), noCollisions(As, B).

/**
 * extractIntervals(Ss, Is): true if Is are the intervals of the Ss
 */
extractIntervals([], []).
extractIntervals([section(_, _, Is) | Ss], O) :- extractIntervals(Ss, O1), append(Is, O1, O).

/**
 * valid(C, S): true if S is the smallest list of sections that don't collide
 * with each other and that contain every course requested
 */
valid([], []).
valid([C | Cs], [section(C, N, Is) | Ss]) :- section(C, N, Is), extractIntervals(Ss, O1), noCollisions(Is, O1), valid(Cs, Ss).