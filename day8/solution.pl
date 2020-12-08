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

solution_part1(Instructions, Solution) :-
  assert_instructions(Instructions),
  run_part1([], 0, 0, Solution).

assert_instructions(Instructions) :-
  nth0(Index, Instructions, In),
  assert(in(Index, In)).

run_part1(Visited, Index, Acc, Result) :-
  member(Index, Visited) ->
    Acc = Result;
  in(Index, nop(_)) ->
    (
      NextIndex is Index + 1,
      run_part1([Index|Visited], NextIndex, Acc, Result)
    );
  in(Index, acc(D)) ->
    (
      NextIndex is Index + 1,
      NextAcc is Acc + D,
      run_part1([Index|Visited], NextIndex, NextAcc, Result)
    );
  in(Index, jmp(D)) ->
    (
      NextIndex is Index + D,
      run_part1([Index|Visited], NextIndex, Acc, Result)
    ).

solution_part2(Instructions, Solution) :-
  assert_instructions(Instructions),
  length(Instructions, FinInstructionIndex),
  assert(in(FinInstructionIndex,fin)),
  run_part2([], replace_available, 0, 0, Solution).

run_part2(Visited, Replace, Index, Acc, Result) :-
  member(Index, Visited) ->
    false;
  in(Index, nop(D)) ->
    (
      (
        NextIndex is Index + 1,
        run_part2([Index|Visited], Replace, NextIndex, Acc, Result)
      );
      (
        Replace = replace_available ->
        (
          NextIndex is Index + D,
          run_part2([Index|Visited], replace_spent, NextIndex, Acc, Result)
        )
      )
    );
  in(Index, acc(D)) ->
    (
      NextIndex is Index + 1,
      NextAcc is Acc + D,
      run_part2([Index|Visited], Replace, NextIndex, NextAcc, Result)
    );
  in(Index, jmp(D)) ->
    (
      (
        NextIndex is Index + D,
        run_part2([Index|Visited], Replace, NextIndex, Acc, Result)
      );
      (
        Replace = replace_available ->
        (
          NextIndex is Index + 1,
          run_part2([Index|Visited], replace_spent, NextIndex, Acc, Result)
        )
      )
    );
  in(Index, fin) ->
    Acc = Result.

parse(Parsed, Raw) :- instructions(Parsed, Raw, []).

instructions([In]) --> instruction(In), `\n`.
instructions([Head|Tail]) --> instruction(Head), `\n`, instructions(Tail).

instruction(nop(N)) --> `nop `, integer(N).
instruction(acc(N)) --> `acc `, integer(N).
instruction(jmp(N)) --> `jmp `, integer(N).
