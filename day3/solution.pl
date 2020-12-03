#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
  open('input', read, Stream),
  lines(Stream,Lines),
  close(Stream),
  solution(Lines,Solution),
  write(Solution),
  halt.
main :-
  halt(1).

lines(Stream,Lines) :-
  read_string(Stream,_,Contents),
  string_concat(Trimmed,"\n",Contents),
  split_string(Trimmed,"\n","",Lines).

solution(Lines, Solution) :-
  % Build knowledge base of the terain.
  maplist(string_chars, Lines, Grid),
  findall(Coord, location(Grid, Coord), Coords),
  grid_width(Grid, Width),
  assert(repetition_width(Width)),
  maplist(assert, Coords),

  % Use knowledge base to find answer.
  findall(vec(Row, Col), slope(Row, Col), Slopes),
  maplist(sleigh, Slopes, Trees),
  productlist(Trees, Solution).

location(Grid,coord(Row, Col, Char)) :-
  nth0(Row, Grid, RowChars),
  nth0(Col, RowChars, Char).

grid_width([Head|_], Width) :-
  length(Head, Width).

slope(1, 1).
slope(1, 3).
slope(1, 5).
slope(1, 7).
slope(2, 1).

sleigh(Slope, Trees) :-
  sleigh_step(vec(0, 0), Slope, 0, Trees).

sleigh_step(Coord, Slope, Acc, Res) :-
  add(Coord, Slope, NextCoord),
  AccInc is Acc + 1,
  (
    looped_coord(Coord, '.') -> sleigh_step(NextCoord, Slope, Acc, Res);
    looped_coord(Coord, '#') -> sleigh_step(NextCoord, Slope, AccInc, Res);
    Acc = Res
  ).

looped_coord(vec(Row, Col), Char) :-
  repetition_width(Width),
  BaseCol is Col mod Width,
  coord(Row, BaseCol, Char).

add(vec(X1, Y1), vec(X2, Y2), vec(XSum, YSum)) :-
  XSum is X1 + X2,
  YSum is Y1 + Y2.

productlist([], 1).
productlist([Head|Tail], Product) :-
  productlist(Tail, Acc),
  Product is Head * Acc.
