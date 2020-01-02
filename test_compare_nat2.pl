% vim: ft=prolog :

:- use_module([copl_compare_nat2]).

:- begin_tests(solve_compare_nat2).
  test(copl10) :-
    solve_compare_nat2_pp("S(S(Z)) is less than S(S(S(Z)))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(Z))) by L-SuccSucc {\n",
      "  S(Z) is less than S(S(Z)) by L-SuccSucc {\n",
      "    Z is less than S(Z) by L-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl13) :-
    solve_compare_nat2_pp("S(S(Z)) is less than S(S(S(S(S(Z)))))", Proof),
    atomics_to_string([
      "S(S(Z)) is less than S(S(S(S(S(Z))))) by L-SuccSucc {\n",
      "  S(Z) is less than S(S(S(S(Z)))) by L-SuccSucc {\n",
      "    Z is less than S(S(S(Z))) by L-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_compare_nat2).

:- run_tests.
:- halt.
