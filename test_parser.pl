% vim: ft=prolog :

:- use_module([copl_parser]).

:- begin_tests(tokenize).
  test(tokenize_empty) :-
    tokenize("", Tokens),
    assertion(Tokens == []).
  test(tokenize_ident_1) :-
    tokenize("foo", Tokens),
    assertion(Tokens == [foo]).
  test(tokenize_ident_2) :-
    tokenize("\tfoo bar2  baz\n", Tokens),
    assertion(Tokens == [foo, bar2, baz]).
:- end_tests(tokenize).

:- run_tests.
:- halt.
