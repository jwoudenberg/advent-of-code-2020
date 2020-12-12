#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- use_module(library(dcg/basics)).

main :-
  read_file_to_codes('input', Raw, []),
  parse(Input, Raw),
  solution_part2(Input, Solution),
  write(Solution),
  halt.
main :-
  halt(1).

solution_part1(Instructions, Solution) :-
  resolve_part1(Instructions, 0, v(0, 0), v(X, Y)),
  Solution is abs(X) + abs(Y).

resolve_part1([], _, Res, Res).
resolve_part1([in('E', Amount)|Tail], Dir, Cur, Res) :-
  from_polar(0, Amount, Cartesian),
  add(Cur, Cartesian, New),
  resolve_part1(Tail, Dir, New, Res).
resolve_part1([in('N', Amount)|Tail], Dir, Cur, Res) :-
  from_polar(270, Amount, Cartesian),
  add(Cur, Cartesian, New),
  resolve_part1(Tail, Dir, New, Res).
resolve_part1([in('W', Amount)|Tail], Dir, Cur, Res) :-
  from_polar(180, Amount, Cartesian),
  add(Cur, Cartesian, New),
  resolve_part1(Tail, Dir, New, Res).
resolve_part1([in('S', Amount)|Tail], Dir, Cur, Res) :-
  from_polar(90, Amount, Cartesian),
  add(Cur, Cartesian, New),
  resolve_part1(Tail, Dir, New, Res).
resolve_part1([in('R', Amount)|Tail], Dir, Cur, Res) :-
  NewDir is mod(Dir + Amount, 360),
  resolve_part1(Tail, NewDir, Cur, Res).
resolve_part1([in('L', Amount)|Tail], Dir, Cur, Res) :-
  NewDir is mod(Dir - Amount, 360),
  resolve_part1(Tail, NewDir, Cur, Res).
resolve_part1([in('F', Amount)|Tail], Dir, Cur, Res) :-
  from_polar(Dir, Amount, Cartesian),
  add(Cur, Cartesian, New),
  resolve_part1(Tail, Dir, New, Res).

from_polar(0, X, v(X, 0)).
from_polar(90, X, v(0, MinX)) :- MinX is -X.
from_polar(180, X, v(MinX, 0)) :- MinX is -X.
from_polar(270, X, v(0, X)).

solution_part2(Instructions, Solution) :-
  resolve_part2(Instructions, v(0, 0), v(10, 1), v(X, Y)),
  Solution is abs(X) + abs(Y).

resolve_part2([], Res, _, Res).
resolve_part2([in('E', Amount)|Tail], Cur, Waypoint, Res) :-
  from_polar(0, Amount, Cartesian),
  add(Waypoint, Cartesian, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('N', Amount)|Tail], Cur, Waypoint, Res) :-
  from_polar(270, Amount, Cartesian),
  add(Waypoint, Cartesian, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('W', Amount)|Tail], Cur, Waypoint, Res) :-
  from_polar(180, Amount, Cartesian),
  add(Waypoint, Cartesian, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('S', Amount)|Tail], Cur, Waypoint, Res) :-
  from_polar(90, Amount, Cartesian),
  add(Waypoint, Cartesian, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('R', Amount)|Tail], Cur, Waypoint, Res) :-
  Arc is mod(Amount, 360),
  rotate(Arc, Waypoint, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('L', Amount)|Tail], Cur, Waypoint, Res) :-
  Arc is mod(-Amount, 360),
  rotate(Arc, Waypoint, New),
  resolve_part2(Tail, Cur, New, Res).
resolve_part2([in('F', Amount)|Tail], Cur, Waypoint, Res) :-
  scalar_prod(Amount, Waypoint, Distance),
  add(Cur, Distance, New),
  resolve_part2(Tail, New, Waypoint, Res).

rotate(0, v(X, Y), v(X, Y)).
rotate(90, v(X, Y), v(Xres, Yres)) :-
  Xres is Y,
  Yres is -X.
rotate(180, v(X, Y), v(Xres, Yres)) :-
  Xres is -X,
  Yres is -Y.
rotate(270, v(X, Y), v(Xres, Yres)) :-
  Xres is -Y,
  Yres is X.

scalar_prod(N, v(X, Y), v(Xres, Yres)) :-
  Xres is X * N,
  Yres is Y * N.

add(v(X1, Y1), v(X2, Y2), v(Xsum, Ysum)) :-
  Xsum is X1 + X2,
  Ysum is Y1 + Y2.

parse(Parsed, Raw) :- instructions(Parsed, Raw, []).

instructions([In]) --> instruction(In).
instructions([In|Tail]) --> instruction(In), instructions(Tail).

instruction(in(Char,Amount)) -->
  [Code], integer(Amount), `\n`,
  { char_code(Char, Code) }.
