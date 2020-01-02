% vim: ft=prolog :

:- module(copl_eval, [
     solve_eval_pp/2,
     solve_eval/2
   ]).
:- use_module([copl_parser, copl_pp, copl_nat]).

solve_eval_pp(Prop, ProofS) :- solve_eval(Prop, Proof), pp_proof(Proof, ProofS).
solve_eval(PropS, Proof) :-
  string(PropS),
  !,
  parse_prop(PropS, Prop),
  solve_eval(Prop, Proof).
solve_eval(Prop, pf(Prop, Proof)) :- solve_eval1(Prop, Proof).
solve_eval1(evalto(nat(N), N), 'E-Const') :- !.
solve_eval1(evalto(plus(E1, E2), N), 'E-Plus'(Proof1, Proof2, Proof3)) :-
  !,
  solve_eval(evalto(E1, N1), Proof1),
  solve_eval(evalto(E2, N2), Proof2),
  solve_nat(plus_is(N1, N2, N), Proof3).
solve_eval1(evalto(times(E1, E2), N), 'E-Times'(Proof1, Proof2, Proof3)) :-
  !,
  solve_eval(evalto(E1, N1), Proof1),
  solve_eval(evalto(E2, N2), Proof2),
  solve_nat(times_is(N1, N2, N), Proof3).
