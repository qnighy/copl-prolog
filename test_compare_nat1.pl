% vim: ft=prolog :

:- use_module([copl_compare_nat1]).

:- begin_tests(solve_compare_nat1).
  test(copl9) :-
    solve_compare_nat1_pp("S(S(Z)) is less than S(S(S(Z)))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(Z))) by L-Succ {}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl12) :-
    solve_compare_nat1_pp("S(S(Z)) is less than S(S(S(S(S(Z)))))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(S(S(Z))))) by L-Trans {\n",
      "  S(S(Z)) is less than S(S(S(S(Z)))) by L-Trans {\n",
      "    S(S(Z)) is less than S(S(S(Z))) by L-Succ {};\n",
      "    S(S(S(Z))) is less than S(S(S(S(Z)))) by L-Succ {}\n",
      "  };\n",
      "  S(S(S(S(Z)))) is less than S(S(S(S(S(Z))))) by L-Succ {}\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_compare_nat1).

:- run_tests.
:- halt.
