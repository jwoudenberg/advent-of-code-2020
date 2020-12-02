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

parsed_line(Line,Pass) :-
  string_chars(Line, Chars),
  password(Pass, Chars, []).

solution(Lines,Solution) :-
  maplist(parsed_line,Lines,AllPasswords),
  include(valid_password_part2, AllPasswords, ValidPasswords),
  length(ValidPasswords,Solution).

password(pass(Min,Max,Char,Pass)) -->
  num(Min),
  ['-'],
  num(Max),
  [' '],
  char(Char),
  [':', ' '],
  string(Pass).
num(Num) --> digits(Chars), {atom_chars(Atom, Chars), atom_number(Atom,Num)}.
digits([Digit]) --> digit(Digit).
digits([Digit|Tail]) --> digit(Digit), digits(Tail).
char(Char) --> [Char].
string([Char]) --> char(Char).
string([Char|Tail]) --> char(Char), string(Tail).
digit(D) --> [D], {atom_number(D, _)}.

valid_password_part1(pass(Min,Max,Char,Pass)) :-
  include(=(Char),Pass,Chars),
  length(Chars, Count),
  Count >= Min,
  Max >= Count.

valid_password_part2(pass(Min,Max,Char,Pass)) :-
  (
    nth1(Min, Pass, Char),
    \+ nth1(Max, Pass, Char)
  );
  (
    nth1(Max, Pass, Char),
    \+ nth1(Min, Pass, Char)
  ).
