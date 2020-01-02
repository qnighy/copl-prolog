% vim: ft=prolog :

:- use_module([copl_pp]).

:- begin_tests(pp).
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
:- end_tests(pp).

:- run_tests.
:- halt.


