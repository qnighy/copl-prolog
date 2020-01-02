% vim: ft=prolog :

:- use_module([copl_eval]).

:- begin_tests(solve_eval).
  test(copl15) :-
    solve_eval_pp("Z + S(S(Z)) evalto S(S(Z))", Proof),
    atomics_to_string([
      "Z + S(S(Z)) evalto S(S(Z)) by E-Plus {\n",
      "  Z evalto Z by E-Const {};\n",
      "  S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "  Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl16) :-
    solve_eval_pp("S(S(Z)) + Z evalto S(S(Z))", Proof),
    atomics_to_string([
      "S(S(Z)) + Z evalto S(S(Z)) by E-Plus {\n",
      "  S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "  Z evalto Z by E-Const {};\n",
      "  S(S(Z)) plus Z is S(S(Z)) by P-Succ {\n",
      "    S(Z) plus Z is S(Z) by P-Succ {\n",
      "      Z plus Z is Z by P-Zero {}\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl17) :-
    solve_eval_pp("S(Z) + S(Z) + S(Z) evalto S(S(S(Z)))", Proof),
    atomics_to_string([
      "S(Z) + S(Z) + S(Z) evalto S(S(S(Z))) by E-Plus {\n",
      "  S(Z) + S(Z) evalto S(S(Z)) by E-Plus {\n",
      "    S(Z) evalto S(Z) by E-Const {};\n",
      "    S(Z) evalto S(Z) by E-Const {};\n",
      "    S(Z) plus S(Z) is S(S(Z)) by P-Succ {\n",
      "      Z plus S(Z) is S(Z) by P-Zero {}\n",
      "    }\n",
      "  };\n",
      "  S(Z) evalto S(Z) by E-Const {};\n",
      "  S(S(Z)) plus S(Z) is S(S(S(Z))) by P-Succ {\n",
      "    S(Z) plus S(Z) is S(S(Z)) by P-Succ {\n",
      "      Z plus S(Z) is S(Z) by P-Zero {}\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl18) :-
    solve_eval_pp("S(S(S(Z))) + S(S(Z)) * S(Z) evalto S(S(S(S(S(Z)))))", Proof),
    atomics_to_string([
      "S(S(S(Z))) + S(S(Z)) * S(Z) evalto S(S(S(S(S(Z))))) by E-Plus {\n",
      "  S(S(S(Z))) evalto S(S(S(Z))) by E-Const {};\n",
      "  S(S(Z)) * S(Z) evalto S(S(Z)) by E-Times {\n",
      "    S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "    S(Z) evalto S(Z) by E-Const {};\n",
      "    S(S(Z)) times S(Z) is S(S(Z)) by T-Succ {\n",
      "      S(Z) times S(Z) is S(Z) by T-Succ {\n",
      "        Z times S(Z) is Z by T-Zero {};\n",
      "        S(Z) plus Z is S(Z) by P-Succ {\n",
      "          Z plus Z is Z by P-Zero {}\n",
      "        }\n",
      "      };\n",
      "      S(Z) plus S(Z) is S(S(Z)) by P-Succ {\n",
      "        Z plus S(Z) is S(Z) by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  };\n",
      "  S(S(S(Z))) plus S(S(Z)) is S(S(S(S(S(Z))))) by P-Succ {\n",
      "    S(S(Z)) plus S(S(Z)) is S(S(S(S(Z)))) by P-Succ {\n",
      "      S(Z) plus S(S(Z)) is S(S(S(Z))) by P-Succ {\n",
      "        Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl19) :-
    solve_eval_pp("(S(S(Z)) + S(S(Z))) * Z evalto Z", Proof),
    atomics_to_string([
      "(S(S(Z)) + S(S(Z))) * Z evalto Z by E-Times {\n",
      "  S(S(Z)) + S(S(Z)) evalto S(S(S(S(Z)))) by E-Plus {\n",
      "    S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "    S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "    S(S(Z)) plus S(S(Z)) is S(S(S(S(Z)))) by P-Succ {\n",
      "      S(Z) plus S(S(Z)) is S(S(S(Z))) by P-Succ {\n",
      "        Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  };\n",
      "  Z evalto Z by E-Const {};\n",
      "  S(S(S(S(Z)))) times Z is Z by T-Succ {\n",
      "    S(S(S(Z))) times Z is Z by T-Succ {\n",
      "      S(S(Z)) times Z is Z by T-Succ {\n",
      "        S(Z) times Z is Z by T-Succ {\n",
      "          Z times Z is Z by T-Zero {};\n",
      "          Z plus Z is Z by P-Zero {}\n",
      "        };\n",
      "        Z plus Z is Z by P-Zero {}\n",
      "      };\n",
      "      Z plus Z is Z by P-Zero {}\n",
      "    };\n",
      "    Z plus Z is Z by P-Zero {}\n",
      "  }\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
  test(copl20) :-
    solve_eval_pp("Z * (S(S(Z)) + S(S(Z))) evalto Z", Proof),
    atomics_to_string([
      "Z * (S(S(Z)) + S(S(Z))) evalto Z by E-Times {\n",
      "  Z evalto Z by E-Const {};\n",
      "  S(S(Z)) + S(S(Z)) evalto S(S(S(S(Z)))) by E-Plus {\n",
      "    S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "    S(S(Z)) evalto S(S(Z)) by E-Const {};\n",
      "    S(S(Z)) plus S(S(Z)) is S(S(S(S(Z)))) by P-Succ {\n",
      "      S(Z) plus S(S(Z)) is S(S(S(Z))) by P-Succ {\n",
      "        Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n",
      "      }\n",
      "    }\n",
      "  };\n",
      "  Z times S(S(S(S(Z)))) is Z by T-Zero {}\n",
      "}"
    ], Expect),
    assertion(Proof == Expect).
:- end_tests(solve_eval).

:- run_tests.
:- halt.
