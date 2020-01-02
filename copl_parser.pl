% vim: ft=prolog :

:- module(copl_parser, [parse_nat/2, tokenize/2]).

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
tokenize([CharsH|CharsT], [CharsH|Tokens]) :-
  tokenize(CharsT, Tokens).

extract_alnum([], [], []) :- !.
extract_alnum([CharsH|CharsT], [CharsH|Alnums], Rem) :-
  char_type(CharsH, ascii),
  char_type(CharsH, alnum),
  !,
  extract_alnum(CharsT, Alnums, Rem).
extract_alnum(Chars, [], Chars).
