#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- use_module(library(dcg/basics)).
:- table spot/4.

main :-
  read_file_to_codes('input', Raw, []),
  parse(Input, Raw),
  solution(Input, Solution),
  write(Solution),
  halt.
main :-
  halt(1).

solution(Input, Solution) :-
  assert_initial_positions(Input),
  stable(0, StableN),
  aggregate_all(
    count,
    ( within_grid(Row, Col),
      spot(StableN, Row, Col, '#')
    ),
    Solution
  ).

stable(N, Stable) :-
  NextN is N + 1,
  (
    (draw_grid(N, Grid), draw_grid(NextN, Grid)) ->
    N = Stable;
    stable(NextN, Stable)
  ).

assert_initial_positions(Grid) :-
  findall(
    init_spot(Row, Col, State),
    (
      nth0(Row, Grid, RowSeats),
      nth0(Col, RowSeats, State)
    ),
    Spots
  ),
  maplist(assert, Spots).

spot(0, Row, Col, State) :-
  init_spot(Row, Col, State).
spot(N, Row, Col, '.') :-
  N > 0,
  init_spot(Row, Col, '.').
spot(N, Row, Col, State) :-
  N > 0,
  \+ init_spot(Row, Col, '.'),
  within_grid(Row, Col),
  PrevN is N - 1,
  occupied_adjacent(PrevN, Row, Col, OccupiedAdjacent),
  (
    OccupiedAdjacent = 0 -> State = '#';
    OccupiedAdjacent >= 5 -> State = 'L';
    spot(PrevN, Row, Col, State)
  ).

occupied_adjacent(N, Row, Col, OccupiedAdjacent) :-
  aggregate_all(
    count,
    (
      direction(Direction),
      seat_visible(N, Row, Col, Direction, '#')
    ),
    OccupiedAdjacent
  ).

within_grid(Row, Col) :-
  init_spot(Row, Col, _).

draw_grid(N, Grid) :-
  draw_helper(N, 0, 0, [], Output),
  reverse(Output, Reversed),
  atomics_to_string(Reversed, Grid).

draw_helper(N, Row, Col, Acc, Res) :-
  spot(N, Row, Col, State) ->
  (
    NextCol is Col + 1,
    draw_helper(N, Row, NextCol, [State|Acc], Res)
  );
  (
    NextRow is Row + 1,
    within_grid(NextRow, 0)
  ) ->
  draw_helper(N, NextRow, 0, ['\n'|Acc], Res);
  Res = ['\n'|Acc].

direction(vector(Drow, Dcol)) :-
  member(Dcol, [1, 0, -1]),
  member(Drow, [1, 0, -1]),
  \+ (Drow = 0, Dcol = 0).

seat_visible(N, Row, Col, Direction, State) :-
  add(vector(Row, Col), Direction, vector(AdjRow, AdjCol)),
  (
    spot(N, AdjRow, AdjCol, '.') ->
    seat_visible(N, AdjRow, AdjCol, Direction, State);
    spot(N, AdjRow, AdjCol, State)
  ).

add(vector(X1, Y1), vector(X2, Y2), vector(Xsum, Ysum)) :-
  Xsum is X1 + X2,
  Ysum is Y1 + Y2.

parse(Parsed, Raw) :- lines(Parsed, Raw, []).

lines([Line]) --> line(Line).
lines([Line|Tail]) --> line(Line), lines(Tail).

line(Line) -->
  string_without(`\n`, Codes), `\n`,
  { maplist(char_code, Line, Codes) }.
