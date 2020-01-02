% vim: ft=prolog :

:- use_module([copl_pp]).

:- begin_tests(pp_proof).
  test(test_pp1) :-
    pp_proof(pf(plus_is(z, z, z), 'P-Zero'), S),
    assertion(S == "plus_is(z,z,z) by P-Zero {}").
  test(test_pp2) :-
    pp_proof(pf(plus_is(s(z), z, s(z)), 'P-Succ'(pf(plus_is(z, z, z), 'P-Zero'))), S),
    atomics_to_string([
      "plus_is(s(z),z,s(z)) by P-Succ {\n",
      "  plus_is(z,z,z) by P-Zero {}\n",
      "}"
    ], Expect),
    assertion(S == Expect).
:- end_tests(pp_proof).

:- begin_tests(pp_prop).
  test(pp_plus_is_0) :-
    pp_prop(plus_is(z, z, z), S),
    assertion(S == "Z plus Z is Z").
  test(pp_plus_is_1) :-
    pp_prop(plus_is(s(z), s(s(z)), s(s(s(z)))), S),
    assertion(S == "S(Z) plus S(S(Z)) is S(S(S(Z)))").
  test(pp_times_is_0) :-
    pp_prop(times_is(z, z, z), S),
    assertion(S == "Z times Z is Z").
  test(pp_times_is_1) :-
    pp_prop(times_is(s(s(z)), s(s(z)), s(s(s(s(z))))), S),
    assertion(S == "S(S(Z)) times S(S(Z)) is S(S(S(S(Z))))").
:- end_tests(pp_prop).

:- begin_tests(pp_nat).
  test(pp_nat_0) :-
    pp_nat(z, S),
    assertion(S == "Z").
  test(pp_nat_1) :-
    pp_nat(s(z), S),
    assertion(S == "S(Z)").
  test(pp_nat_2) :-
    pp_nat(s(s(z)), S),
    assertion(S == "S(S(Z))").
:- end_tests(pp_nat).

:- run_tests.
:- halt.


