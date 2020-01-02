% vim: ft=prolog :

:- use_module([copl_parser]).

:- begin_tests(parse_prop).
  test(parse_plus_is_0) :-
    parse_prop("Z plus Z is Z", Prop),
    assertion(Prop == plus_is(z, z, z)).
  test(parse_plus_is_1) :-
    parse_prop("S(Z) plus S(S(Z)) is S(S(S(Z)))", Prop),
    assertion(Prop == plus_is(s(z), s(s(z)), s(s(s(z))))).
  test(parse_times_is_0) :-
    parse_prop("Z times Z is Z", Prop),
    assertion(Prop == times_is(z, z, z)).
  test(parse_times_is_1) :-
    parse_prop("S(S(Z)) times S(S(Z)) is S(S(S(S(Z))))", Prop),
    assertion(Prop == times_is(s(s(z)), s(s(z)), s(s(s(s(z)))))).
  test(parse_is_less_than_0) :-
    parse_prop("Z is less than Z", Prop),
    assertion(Prop == is_less_than(z, z)).
  test(parse_is_less_than_1) :-
    parse_prop("S(S(Z)) is less than S(S(S(Z)))", Prop),
    assertion(Prop == is_less_than(s(s(z)), s(s(s(z))))).
  test(parse_evalto) :-
    parse_prop("Z + Z evalto Z", Prop),
    assertion(Prop == evalto(plus(nat(z), nat(z)), z)).
:- end_tests(parse_prop).

:- begin_tests(parse_exp).
  test(parse_exp_nat_0) :-
    parse_exp("Z", E),
    assertion(E == nat(z)).
  test(parse_exp_nat_1) :-
    parse_exp("S(Z)", E),
    assertion(E == nat(s(z))).
  test(parse_exp_plus_0) :-
    parse_exp("S(S(Z)) + Z", E),
    assertion(E == plus(nat(s(s(z))), nat(z))).
  test(parse_exp_plus_1) :-
    parse_exp("Z + S(Z) + Z", E),
    assertion(E == plus(plus(nat(z), nat(s(z))), nat(z))).
  test(parse_exp_plus_2) :-
    parse_exp("Z + (S(Z) + Z)", E),
    assertion(E == plus(nat(z), plus(nat(s(z)), nat(z)))).
  test(parse_exp_times_0) :-
    parse_exp("(Z + S(Z)) * Z", E),
    assertion(E == times(plus(nat(z), nat(s(z))), nat(z))).
  test(parse_exp_times_1) :-
    parse_exp("Z + S(Z) * Z", E),
    assertion(E == plus(nat(z), times(nat(s(z)), nat(z)))).
:- end_tests(parse_exp).

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
