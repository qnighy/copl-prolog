% vim: ft=prolog :

:- use_module([copl_parser]).

:- begin_tests(parse_nat).
  test(parse_nat_0) :-
    parse_nat("Z", N),
    assertion(N == z).
  test(parse_nat_1) :-
    parse_nat("S(Z)", N),
    assertion(N == s(z)).
  test(parse_nat_2) :-
    parse_nat("S(S(Z))", N),
    assertion(N == s(s(z))).
:- end_tests(parse_nat).

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
