% vim: ft=prolog :

:- module(copl_compare_nat1, [
     solve_compare_nat1_pp/2,
     solve_compare_nat1/2
   ]).
:- use_module([copl_parser, copl_pp]).

solve_compare_nat1_pp(Prop, ProofS) :-
  solve_compare_nat1(Prop, Proof),
  pp_proof(Proof, ProofS).
solve_compare_nat1(PropS, Proof) :-
  string(PropS),
  !,
  parse_prop(PropS, Prop),
  solve_compare_nat1(Prop, Proof).
solve_compare_nat1(Prop, Proof) :- solve_impl(Prop, Proof), !.
solve_impl(Prop, pf(Prop, Proof)) :- solve_impl1(Prop, Proof).
solve_impl1(is_less_than(N, s(N)), 'L-Succ').
solve_impl1(is_less_than(_, z), _) :- !, fail.
solve_impl1(is_less_than(N1, N3), 'L-Trans'(Proof1, Proof2)) :-
  solve_impl(is_less_than(N2, N3), Proof2),
  solve_impl(is_less_than(N1, N2), Proof1).
