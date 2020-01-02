% vim: ft=prolog :

:- use_module([copl_nat]).

:- begin_tests(solve_nat).
  test(copl1) :-
    solve_nat_pp("Z plus Z is Z", Proof),
    atomics_to_string([
      "Z plus Z is Z by P-Zero {}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl2) :-
    solve_nat_pp("Z plus S(S(Z)) is S(S(Z))", Proof),
    atomics_to_string([
      "Z plus S(S(Z)) is S(S(Z)) by P-Zero {}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl3) :-
    solve_nat_pp("S(S(Z)) plus Z is S(S(Z))", Proof),
    atomics_to_string([
      "S(S(Z)) plus Z is S(S(Z)) by P-Succ {\n",
      "  S(Z) plus Z is S(Z) by P-Succ {\n",
      "    Z plus Z is Z by P-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl4) :-
    solve_nat_pp("S(Z) plus S(S(S(Z))) is S(S(S(S(Z))))", Proof),
    atomics_to_string([
      "S(Z) plus S(S(S(Z))) is S(S(S(S(Z)))) by P-Succ {\n",
      "  Z plus S(S(S(Z))) is S(S(S(Z))) by P-Zero {}\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl5) :-
    solve_nat_pp("Z times S(S(Z)) is Z", Proof),
    atomics_to_string([
      "Z times S(S(Z)) is Z by T-Zero {}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl6) :-
    solve_nat_pp("S(S(Z)) times Z is Z", Proof),
    atomics_to_string([
      "S(S(Z)) times Z is Z by T-Succ {\n",
      "  S(Z) times Z is Z by T-Succ {\n",
      "    Z times Z is Z by T-Zero {};\n",
      "    Z plus Z is Z by P-Zero {}\n",
      "  };\n",
      "  Z plus Z is Z by P-Zero {}\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl7) :-
    solve_nat_pp("S(S(Z)) times S(Z) is S(S(Z))", Proof),
    atomics_to_string([
      "S(S(Z)) times S(Z) is S(S(Z)) by T-Succ {\n",
      "  S(Z) times S(Z) is S(Z) by T-Succ {\n",
      "    Z times S(Z) is Z by T-Zero {};\n",
      "    S(Z) plus Z is S(Z) by P-Succ {\n",
      "      Z plus Z is Z by P-Zero {}\n",
      "    }\n",
      "  };\n",
      "  S(Z) plus S(Z) is S(S(Z)) by P-Succ {\n",
      "    Z plus S(Z) is S(Z) by P-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl8) :-
    solve_nat_pp("S(S(Z)) times S(S(Z)) is S(S(S(S(Z))))", Proof),
    atomics_to_string([
      "S(S(Z)) times S(S(Z)) is S(S(S(S(Z)))) by T-Succ {\n",
      "  S(Z) times S(S(Z)) is S(S(Z)) by T-Succ {\n",
      "    Z times S(S(Z)) is Z by T-Zero {};\n",
      "    S(S(Z)) plus Z is S(S(Z)) by P-Succ {\n",
      "      S(Z) plus Z is S(Z) by P-Succ {\n",
      "        Z plus Z is Z by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  };\n",
      "  S(S(Z)) plus S(S(Z)) is S(S(S(S(Z)))) by P-Succ {\n",
      "    S(Z) plus S(S(Z)) is S(S(S(Z))) by P-Succ {\n",
      "      Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_nat).

:- run_tests.
:- halt.
