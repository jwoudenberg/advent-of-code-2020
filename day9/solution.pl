#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- use_module(library(dcg/basics)).

main :-
  read_file_to_codes('input', Raw, []),
  parse(Input, Raw),
  solution(Input, Solution),
  write(Solution),
  halt.
main :-
  halt(1).

solution(Stream, Solution) :-
  first_invalid(25, Stream, TargetSum),
  infix(SubList, Stream),
  length(SubList, SubLength),
  SubLength >= 2,
  sumlist(SubList, TargetSum),
  min_member(SubMin, SubList),
  max_member(SubMax, SubList),
  Solution is SubMin + SubMax.

first_invalid(Preamble, [Head|Tail], Invalid) :-
  valid_head(Preamble, [Head|Tail]) ->
    first_invalid(Preamble, Tail, Invalid);
    nth0(Preamble, [Head|Tail], Invalid).

valid_head(Preamble, Stream) :-
  nth0(Preamble, Stream, N),
  nth0(IndexPrev1, Stream, Prev1),
  nth0(IndexPrev2, Stream, Prev2),
  IndexPrev1 < Preamble,
  IndexPrev2 < Preamble,
  \+ IndexPrev1 = IndexPrev2,
  N is Prev1 + Prev2.

infix(Infix, Full) :-
  append(Part, _Suffix, Full),
  append(_Prefix, Infix, Part).

parse(Parsed, Raw) :- numbers(Parsed, Raw, []).

numbers([N]) --> integer(N), `\n`.
numbers([N|Tail]) --> integer(N), `\n`, numbers(Tail).
