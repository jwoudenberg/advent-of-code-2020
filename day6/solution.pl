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

solution_part1(Groups, Solution) :-
  maplist(unique_answers, Groups, AnswerCounts),
  sumlist(AnswerCounts, Solution).

unique_answers(Group, AnswerCount) :-
  flatten(Group, DupedAnswers),
  list_to_set(DupedAnswers, Answers),
  length(Answers, AnswerCount).

solution_part2(Groups, Solution) :-
  maplist(shared_answers, Groups, AnswerCounts),
  sumlist(AnswerCounts, Solution).

shared_answers(Groups, AnswerCount) :-
  findall(X, maplist(member(X), Groups), Answers),
  length(Answers, AnswerCount).

parse(Parsed, Raw) :- groups(Parsed, Raw, []).

groups([Group]) --> group(Group).
groups([Head|Tail]) --> group(Head), `\n`, groups(Tail).

group([Person]) --> answers(Person).
group([Head|Tail]) --> answers(Head), group(Tail).

answers([Head|Tail]) --> string_without(`\n`, [Head|Tail]), `\n`.
