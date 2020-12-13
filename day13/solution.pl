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

solution_part1(service(Start, BusesAndXs), Solution) :-
  exclude(=(x), BusesAndXs, Buses),
  maplist(next_departure(Start), Buses, Departures),
  sort(2, @=<, Departures, [departure(Bus, Time)|_]),
  Solution is Bus * Time.

next_departure(Start, Bus, departure(Bus, Time)) :-
  Time is Bus - mod(Start, Bus).

solution_part2(service(_, BusesAndXs), Solution) :-
  findall(
    series(Rem, Div), % equation: t = Div % Rem
    (
      nth0(Index, BusesAndXs, Div),
      \+ Div = x,
      Rem is mod(Div - Index, Div)
    ),
    Equations
  ),
  sort(2, @>=, Equations, SortedEquations),
  intersect_list(SortedEquations, series(Solution, _)).

% values(series(A, N), X) is true if X is in the series: A + N*k
values(series(Base, _), Base).
values(series(Base, Step), Val) :-
  values(series(Base, Step), Prev),
  Val is Prev + Step.

% intersect(X, Y, Z) means series Z contains values that appear in both X and Y.
intersect(series(X0, N0), series(X1, N1), series(Xres, Nres)) :-
  Nres is N0 * N1,
  once(
    (
      values(series(X0, N0), Xres),
      X1 is Xres mod N1
    )
  ).

% Fold a list of series into one using `intersect`.
intersect_list([Series], Series).
intersect_list([X,Y|Tail], Res) :-
  intersect(X, Y, Z),
  intersect_list([Z|Tail], Res).

parse(Service, Raw) :- service(Service, Raw, []).

service(service(Start, Buses)) --> integer(Start), `\n`, buses(Buses).

buses([Bus]) --> bus(Bus), `\n`.
buses([Bus|Tail]) --> bus(Bus), `,`, buses(Tail).

bus(x) --> `x`.
bus(N) --> integer(N).
