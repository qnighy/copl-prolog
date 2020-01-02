% vim: ft=prolog :

:- use_module([copl_pp]).

:- begin_tests(pp_proof).
  test(proof_plus_is_1) :-
    pp_proof(pf(plus_is(z, z, z), 'P-Zero'), S),
    assertion(S == "Z plus Z is Z by P-Zero {}").
  test(proof_plus_is_2) :-
    pp_proof(pf(plus_is(s(z), z, s(z)), 'P-Succ'(pf(plus_is(z, z, z), 'P-Zero'))), S),
    atomics_to_string([
      "S(Z) plus Z is S(Z) by P-Succ {\n",
      "  Z plus Z is Z by P-Zero {}\n",
      "}"
    ], Expect),
    assertion(S == Expect).
  test(proof_times_is_1) :-
    pp_proof(pf(times_is(z, z, z), 'T-Zero'), S),
    assertion(S == "Z times Z is Z by T-Zero {}").
  test(proof_times_is_2) :-
    pp_proof(pf(times_is(s(z), z, z), 'T-Succ'(pf(times_is(z, z, z), 'T-Zero'), pf(plus_is(z, z, z), 'P-Zero'))), S),
    atomics_to_string([
      "S(Z) times Z is Z by T-Succ {\n",
      "  Z times Z is Z by T-Zero {};\n",
      "  Z plus Z is Z by P-Zero {}\n",
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
  test(pp_is_less_than_0) :-
    pp_prop(is_less_than(z, s(z)), S),
    assertion(S == "Z is less than S(Z)").
  test(pp_evalto) :-
    pp_prop(evalto(plus(nat(z), nat(z)), z), S),
    assertion(S == "Z + Z evalto Z").
  test(pp_reduce) :-
    pp_prop(reduce(plus(nat(z), nat(z)), nat(z)), S),
    assertion(S == "Z + Z ---> Z").
  test(pp_reduce_d) :-
    pp_prop(reduce_d(plus(nat(z), nat(z)), nat(z)), S),
    assertion(S == "Z + Z -d-> Z").
  test(pp_reduce_m) :-
    pp_prop(reduce_m(plus(nat(z), nat(z)), nat(z)), S),
    assertion(S == "Z + Z -*-> Z").
:- end_tests(pp_prop).

:- begin_tests(pp_exp).
  test(pp_exp_nat_0) :-
    pp_exp(nat(z), S),
    assertion(S == "Z").
  test(pp_exp_nat_1) :-
    pp_exp(nat(s(z)), S),
    assertion(S == "S(Z)").
  test(pp_exp_plus_0) :-
    pp_exp(plus(nat(s(s(z))), nat(z)), S),
    assertion(S == "S(S(Z)) + Z").
  test(pp_exp_plus_1) :-
    pp_exp(plus(plus(nat(z), nat(s(z))), nat(z)), S),
    assertion(S == "Z + S(Z) + Z").
  test(pp_exp_plus_2) :-
    pp_exp(plus(nat(z), plus(nat(s(z)), nat(z))), S),
    assertion(S == "Z + (S(Z) + Z)").
  test(pp_exp_times_0) :-
    pp_exp(times(plus(nat(z), nat(s(z))), nat(z)), S),
    assertion(S == "(Z + S(Z)) * Z").
  test(pp_exp_times_1) :-
    pp_exp(plus(nat(z), times(nat(s(z)), nat(z))), S),
    assertion(S == "Z + S(Z) * Z").
:- end_tests(pp_exp).

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


