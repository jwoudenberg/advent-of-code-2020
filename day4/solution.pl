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

solution(Passports, Solution) :-
  include(valid_passport, Passports, ValidPassports),
  length(ValidPassports, Solution).

valid_passport(Passport) :-
  include(valid_byr, Passport, [_]),
  include(valid_iyr, Passport, [_]),
  include(valid_eyr, Passport, [_]),
  include(valid_hgt, Passport, [_]),
  include(valid_hcl, Passport, [_]),
  include(valid_ecl, Passport, [_]),
  include(valid_pid, Passport, [_]).

valid_byr(kv(byr, Val)) :-
  number_codes(Year, Val),
  Year >= 1920,
  2002 >= Year.

valid_iyr(kv(iyr, Val)) :-
  number_codes(Year, Val),
  Year >= 2010,
  2020 >= Year.

valid_eyr(kv(eyr, Val)) :-
  number_codes(Year, Val),
  Year >= 2020,
  2030 >= Year.

valid_hgt(kv(hgt, Val)) :- valid_height(Val, []).

valid_height --> integer(Height), `cm`, { Height >= 150, 193 >= Height }.
valid_height --> integer(Height), `in`, { Height >= 59, 76 >= Height }.

valid_hcl(kv(hcl, Val)) :- valid_hcl(Val, []).

valid_hcl --> `#`, xdigits(Digits), { length(Digits, 6) }.

valid_ecl(kv(ecl,`amb`)).
valid_ecl(kv(ecl,`blu`)).
valid_ecl(kv(ecl,`brn`)).
valid_ecl(kv(ecl,`gry`)).
valid_ecl(kv(ecl,`grn`)).
valid_ecl(kv(ecl,`hzl`)).
valid_ecl(kv(ecl,`oth`)).

valid_pid(kv(pid, Val)) :- valid_pid(Val, []).

valid_pid --> digits(Digits), { length(Digits, 9) }.

parse(Parsed, Raw) :- passports(Parsed, Raw, []).

passports([Passport]) --> pairs(Passport).
passports([Head|Tail]) --> pairs(Head), `\n`, passports(Tail).

pairs([KeyValue]) --> pair(KeyValue).
pairs([Head|Tail]) --> pair(Head), pairs(Tail).

pair(kv(Key,Val)) -->
  string_without(` \n:`, KeyCodes), `:`, string_without(` \n`, Val), blank,
  { atom_codes(Key, KeyCodes) }.
