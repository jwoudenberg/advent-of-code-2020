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

solution_part1(Seats, Solution) :-
  maplist(seat_id, Seats, SeatIds),
  max_member(Solution, SeatIds).

solution_part2(Seats, Solution) :-
  maplist(seat_id, Seats, SeatIds),
  member(Prev, SeatIds),
  Solution is Prev + 1,
  \+ member(Solution, SeatIds),
  Next is Solution + 1,
  member(Next, SeatIds).

seat_id(seat(Row, Col), Id) :-
  Id is Col + (Row * 8).

parse(Parsed, Raw) :- seats(Parsed, Raw, []).

seats([Seat]) --> seat_line(Seat).
seats([Head|Tail]) --> seat_line(Head), seats(Tail).

seat_line(seat(Row, Col)) --> row(Row), col(Col), `\n`.
row(Row) -->
  string_without(`RL`, RowCodes),
  { maplist(fb_binary, RowCodes, Digits), binary_number(Digits, Row) }.
col(Col) --> string_without(`\n`, ColCodes),
  { maplist(rl_binary, ColCodes, Digits), binary_number(Digits, Col) }.

fb_binary(0'F, 0'0).
fb_binary(0'B, 0'1).

rl_binary(0'L, 0'0).
rl_binary(0'R, 0'1).

binary_number(Digits, Number) :-
  append(`0b`, Digits, Binary),
  number_codes(Number, Binary).
