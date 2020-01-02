% vim: ft=prolog :

:- module(copl_compare_nat2, [
     solve_compare_nat2_pp/2,
     solve_compare_nat2/2
   ]).
:- use_module([copl_parser, copl_pp]).

solve_compare_nat2_pp(Prop, ProofS) :-
  solve_compare_nat2(Prop, Proof),
  pp_proof(Proof, ProofS).
solve_compare_nat2(PropS, Proof) :-
  string(PropS),
  !,
  parse_prop(PropS, Prop),
  solve_compare_nat2(Prop, Proof).
solve_compare_nat2(Prop, Proof) :- solve_impl(Prop, Proof), !.
solve_impl(Prop, pf(Prop, Proof)) :- solve_impl1(Prop, Proof).
solve_impl1(is_less_than(z, s(_)), 'L-Zero') :- !.
solve_impl1(is_less_than(s(N1), s(N2)), 'L-SuccSucc'(Proof1)) :-
  !,
  solve_impl(is_less_than(N1, N2), Proof1).
