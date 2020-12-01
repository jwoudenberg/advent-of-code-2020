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

solution(Lines,Solution) :-
  member(X,Lines),
  member(Y,Lines),
  member(Z,Lines),
  atom_number(X, XN),
  atom_number(Y, YN),
  atom_number(Z, ZN),
  2020 is XN + YN + ZN,
  Solution is XN * YN * ZN.
