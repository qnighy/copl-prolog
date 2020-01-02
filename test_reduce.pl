% vim: ft=prolog :

:- use_module([copl_reduce]).

:- begin_tests(solve_reduce).
  test(copl21) :-
    solve_reduce_pp("Z + S(S(Z)) -*-> S(S(Z))", Proof),
    atomics_to_string([
      "Z + S(S(Z)) -*-> S(S(Z)) by MR-One {\n",
      "  Z + S(S(Z)) ---> S(S(Z)) by R-Plus {\n",
      "    Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl22) :-
    solve_reduce_pp("S(Z) * S(Z) + S(Z) * S(Z) -d-> S(Z) + S(Z) * S(Z)", Proof),
    atomics_to_string([
      "S(Z) * S(Z) + S(Z) * S(Z) -d-> S(Z) + S(Z) * S(Z) by DR-PlusL {\n",
      "  S(Z) * S(Z) -d-> S(Z) by DR-Times {\n",
      "    S(Z) times S(Z) is S(Z) by T-Succ {\n",
      "      Z times S(Z) is Z by T-Zero {};\n",
      "      S(Z) plus Z is S(Z) by P-Succ {\n",
      "        Z plus Z is Z by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl23) :-
    solve_reduce_pp("S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) * S(Z) + S(Z)", Proof),
    atomics_to_string([
      "S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) * S(Z) + S(Z) by R-PlusR {\n  S(Z) * S(Z) ---> S(Z) by R-Times {\n    S(Z) times S(Z) is S(Z) by T-Succ {\n      Z times S(Z) is Z by T-Zero {};\n      S(Z) plus Z is S(Z) by P-Succ {\n        Z plus Z is Z by P-Zero {}\n      }\n    }\n  }\n}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl24) :-
    solve_reduce_pp("S(Z) * S(Z) + S(Z) * S(Z) -*-> S(S(Z))", Proof),
    atomics_to_string([
      "S(Z) * S(Z) + S(Z) * S(Z) -*-> S(S(Z)) by MR-Multi {\n",
      "  S(Z) * S(Z) + S(Z) * S(Z) -*-> S(Z) + S(Z) * S(Z) by MR-One {\n",
      "    S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) + S(Z) * S(Z) by R-PlusL {\n",
      "      S(Z) * S(Z) ---> S(Z) by R-Times {\n",
      "        S(Z) times S(Z) is S(Z) by T-Succ {\n",
      "          Z times S(Z) is Z by T-Zero {};\n",
      "          S(Z) plus Z is S(Z) by P-Succ {\n",
      "            Z plus Z is Z by P-Zero {}\n",
      "          }\n",
      "        }\n",
      "      }\n",
      "    }\n",
      "  };\n",
      "  S(Z) + S(Z) * S(Z) -*-> S(S(Z)) by MR-Multi {\n",
      "    S(Z) + S(Z) * S(Z) -*-> S(Z) + S(Z) by MR-One {\n",
      "      S(Z) + S(Z) * S(Z) ---> S(Z) + S(Z) by R-PlusR {\n",
      "        S(Z) * S(Z) ---> S(Z) by R-Times {\n",
      "          S(Z) times S(Z) is S(Z) by T-Succ {\n",
      "            Z times S(Z) is Z by T-Zero {};\n",
      "            S(Z) plus Z is S(Z) by P-Succ {\n",
      "              Z plus Z is Z by P-Zero {}\n",
      "            }\n",
      "          }\n",
      "        }\n",
      "      }\n",
      "    };\n",
      "    S(Z) + S(Z) -*-> S(S(Z)) by MR-One {\n",
      "      S(Z) + S(Z) ---> S(S(Z)) by R-Plus {\n",
      "        S(Z) plus S(Z) is S(S(Z)) by P-Succ {\n",
      "          Z plus S(Z) is S(Z) by P-Zero {}\n",
      "        }\n",
      "      }\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_reduce).

:- run_tests.
:- halt.
