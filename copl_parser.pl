% vim: ft=prolog :

:- module(copl_parser, [tokenize/2]).

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
