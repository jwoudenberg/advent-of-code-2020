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

solution_part1(Rules, Solution) :-
  maplist(assert, Rules),
  findall(X, contains_color_rec(bag(shiny, gold), X), DupedBags),
  list_to_set(DupedBags, Bags),
  length(Bags, Solution).

contains_color_rec(Inner, Outer) :- contains_color(Inner, Outer).
contains_color_rec(Inner, Outer) :-
  contains_color(Inner, Middle),
  contains_color_rec(Middle, Outer).

contains_color(Inner, Outer) :-
  contains(Outer, AllInner),
  member(bags(_, Inner), AllInner).

solution_part2(Rules, Solution) :-
  maplist(assert, Rules),
  count_contents(bag(shiny, gold), Solution).

count_contents(Bag, Count) :-
  contains(Bag, Inner),
  maplist(count_helper, Inner, ContentsCount),
  sumlist(ContentsCount, Count).

count_helper(bags(BagCount, Inner), Count) :-
  count_contents(Inner, ContentsCount),
  Count is BagCount * (1 + ContentsCount).

parse(Parsed, Raw) :- rules(Parsed, Raw, []).

rules([Rule]) --> rule(Rule).
rules([Head|Tail]) --> rule(Head), rules(Tail).

rule(contains(Outer, Inner)) -->
  parse_bag(Outer),
  ` bags contain `,
  inner_bags(Inner),
  `.\n`.

inner_bags([]) --> `no other bags`.
inner_bags([Bag]) --> inner_bag(Bag).
inner_bags([Head|Tail]) --> inner_bag(Head), `, `, inner_bags(Tail).

inner_bag(bags(1, Bag)) --> `1 `, parse_bag(Bag), ` bag`.
inner_bag(bags(Quantity, Bag)) -->
  integer(Quantity),
  ` `,
  parse_bag(Bag),
  ` bags`.

parse_bag(bag(Adjective, Color)) -->
  string_without(` `, AdjectiveCodes),
  ` `,
  string_without(` `, ColorCodes),
  { atom_codes(Adjective, AdjectiveCodes), atom_codes(Color, ColorCodes) }.
