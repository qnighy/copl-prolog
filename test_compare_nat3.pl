% vim: ft=prolog :

:- use_module([copl_compare_nat3]).

:- begin_tests(solve_compare_nat3).
  test(copl11) :-
    solve_compare_nat3_pp("S(S(Z)) is less than S(S(S(Z)))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(Z))) by L-Succ {}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl14) :-
    solve_compare_nat3_pp("S(S(Z)) is less than S(S(S(S(S(Z)))))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(S(S(Z))))) by L-SuccR {\n",
      "  S(S(Z)) is less than S(S(S(S(Z)))) by L-SuccR {\n",
      "    S(S(Z)) is less than S(S(S(Z))) by L-Succ {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_compare_nat3).

:- run_tests.
:- halt.
