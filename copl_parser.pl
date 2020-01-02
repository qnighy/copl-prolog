% vim: ft=prolog :

:- module(copl_parser, [parse_prop/2, parse_exp/2, parse_nat/2, tokenize/2]).

parse_prop(S, Prop) :-
  string(S),
  !,
  tokenize(S, Tokens),
  parse_prop(Tokens, Prop).
parse_prop(Tokens, Prop) :- parse_prop(Tokens, Prop, []).
parse_prop(Tokens, plus_is(N1, N2, N3), Rem) :-
  parse_nat(Tokens, N1, ['plus'|Rem1]),
  parse_nat(Rem1, N2, ['is'|Rem2]),
  parse_nat(Rem2, N3, Rem),
  !.
parse_prop(Tokens, times_is(N1, N2, N3), Rem) :-
  parse_nat(Tokens, N1, ['times'|Rem1]),
  parse_nat(Rem1, N2, ['is'|Rem2]),
  parse_nat(Rem2, N3, Rem),
  !.
parse_prop(Tokens, is_less_than(N1, N2), Rem) :-
  parse_nat(Tokens, N1, ['is', 'less', 'than'|Rem1]),
  parse_nat(Rem1, N2, Rem),
  !.
parse_prop(Tokens, evalto(E, N), Rem) :-
  parse_exp(Tokens, E, ['evalto'|Rem1]),
  parse_nat(Rem1, N, Rem),
  !.
parse_prop(Tokens, reduce(E1, E2), Rem) :-
  parse_exp(Tokens, E1, ['--->'|Rem1]),
  parse_exp(Rem1, E2, Rem),
  !.
parse_prop(Tokens, reduce_d(E1, E2), Rem) :-
  parse_exp(Tokens, E1, ['-d->'|Rem1]),
  parse_exp(Rem1, E2, Rem),
  !.
parse_prop(Tokens, reduce_m(E1, E2), Rem) :-
  parse_exp(Tokens, E1, ['-*->'|Rem1]),
  parse_exp(Rem1, E2, Rem),
  !.

parse_exp(S, E) :-
  string(S),
  !,
  tokenize(S, Tokens),
  parse_exp(Tokens, E).
parse_exp(Tokens, N) :- parse_exp(Tokens, N, []).
parse_exp(Tokens, E, Rem) :- parse_exp_plus(Tokens, E, Rem).
parse_exp_plus(Tokens, E, Rem) :-
  parse_exp_times(Tokens, E1, Rem1),
  parse_exp_plus_cont(Rem1, E1, E, Rem).
parse_exp_plus_cont(['+'|Tokens], EL, E, Rem) :-
  !,
  parse_exp_times(Tokens, ER, Rem1),
  parse_exp_plus_cont(Rem1, plus(EL, ER), E, Rem).
parse_exp_plus_cont(Tokens, E, E, Tokens).
parse_exp_times(Tokens, E, Rem) :-
  parse_exp_atom(Tokens, E1, Rem1),
  parse_exp_times_cont(Rem1, E1, E, Rem).
parse_exp_times_cont(['*'|Tokens], EL, E, Rem) :-
  !,
  parse_exp_atom(Tokens, ER, Rem1),
  parse_exp_times_cont(Rem1, times(EL, ER), E, Rem).
parse_exp_times_cont(Tokens, E, E, Tokens).
parse_exp_atom(['('|Tokens], E, Rem) :-
  !,
  parse_exp(Tokens, E, [')'|Rem]).
parse_exp_atom(Tokens, nat(N), Rem) :-
  (Tokens = ['Z'|_]; Tokens = ['S'|_]),
  !,
  parse_nat(Tokens, N, Rem).

parse_nat(S, N) :-
  string(S),
  !,
  tokenize(S, Tokens),
  parse_nat(Tokens, N).
parse_nat(Tokens, N) :- parse_nat(Tokens, N, []).
parse_nat(['Z'|Rem], z, Rem) :- !.
parse_nat(['S', '('|Tokens], s(N), Rem) :-
  !,
  parse_nat(Tokens, N, [')'|Rem]).

tokenize(S, Tokens) :-
  string(S),
  !,
  string_chars(S, Chars),
  tokenize(Chars, Tokens).

tokenize([], []).
tokenize([CharsH|CharsT], Tokens) :-
  char_type(CharsH, ascii),
  char_type(CharsH, space),
  !,
  tokenize(CharsT, Tokens).
tokenize([CharsH|CharsT], [Ident|Tokens]) :-
  char_type(CharsH, ascii),
  char_type(CharsH, alpha),
  !,
  extract_alnum(CharsT, Alnums, Rem),
  atomic_list_concat([CharsH|Alnums], Ident),
  tokenize(Rem, Tokens).
tokenize(['-', '-', '-', '>'|CharsT], ['--->'|Tokens]) :-
  !,
  tokenize(CharsT, Tokens).
tokenize(['-', 'd', '-', '>'|CharsT], ['-d->'|Tokens]) :-
  !,
  tokenize(CharsT, Tokens).
tokenize(['-', '*', '-', '>'|CharsT], ['-*->'|Tokens]) :-
  !,
  tokenize(CharsT, Tokens).
tokenize([CharsH|CharsT], [CharsH|Tokens]) :-
  tokenize(CharsT, Tokens).

extract_alnum([], [], []) :- !.
extract_alnum([CharsH|CharsT], [CharsH|Alnums], Rem) :-
  char_type(CharsH, ascii),
  char_type(CharsH, alnum),
  !,
  extract_alnum(CharsT, Alnums, Rem).
extract_alnum(Chars, [], Chars).
