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

solution_part1(Adapters, Solution) :-
  max_member(MaxJoltage, Adapters),
  DeviceJoltage is MaxJoltage + 3,
  sort([0,DeviceJoltage|Adapters], SortedDevices),
  findall(D, (nextto(X, Y, SortedDevices), D is Y - X), JoltageDiffs),
  aggregate_all(count, member(1, JoltageDiffs), OneCount),
  aggregate_all(count, member(3, JoltageDiffs), ThreeCount),
  Solution is OneCount * ThreeCount.

solution_part2(Adapters, Solution) :-
  max_member(MaxJoltage, Adapters),
  DeviceJoltage is MaxJoltage + 3,
  sort([0,DeviceJoltage|Adapters], SortedDevices),
  findall(D, (nextto(X, Y, SortedDevices), D is Y - X), JoltageDiffs),
  split_on_threes(JoltageDiffs, [], ThreelessRegions),
  maplist(simplifications, ThreelessRegions, Counts),
  productlist(Counts, Solution).

split_on_threes([], Acc, [Acc]).
split_on_threes([3|Tail], Acc, [Acc|Res]) :-
  split_on_threes(Tail, [], Res).
split_on_threes([1|Tail], Acc, Res) :-
  split_on_threes(Tail, [1|Acc], Res).
split_on_threes([2|Tail], Acc, Res) :-
  split_on_threes(Tail, [2|Acc], Res).

simplifications(X, Count) :-
  findall(Y, simplify(X, Y), Simplifications),
  sort(Simplifications, UniqueSimplifications),
  length(UniqueSimplifications, WithoutOriginalCount),
  Count is WithoutOriginalCount + 1.

simplify(X, Y) :- simplify_helper(X, Y).
simplify(X, Y) :-
  simplify_helper(X, Z),
  simplify(Z, Y).

simplify_helper([1,1|Tail], [2|Tail]).
simplify_helper([1,2|Tail], [3|Tail]).
simplify_helper([2,1|Tail], [3|Tail]).
simplify_helper([H|Tail], [H|Simplified]) :- simplify(Tail, Simplified).

productlist([], 1).
productlist([Head|Tail], Product) :-
  productlist(Tail, Acc),
  Product is Head * Acc.

parse(Parsed, Raw) :- numbers(Parsed, Raw, []).

numbers([N]) --> integer(N), `\n`.
numbers([N|Tail]) --> integer(N), `\n`, numbers(Tail).
