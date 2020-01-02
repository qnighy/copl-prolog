% vim: ft=prolog :

:- module(copl_nat, [
     solve_nat_pp/2,
     solve_nat/2
   ]).
:- use_module([copl_parser, copl_pp]).

solve_nat_pp(Prop, ProofS) :- solve_nat(Prop, Proof), pp_proof(Proof, ProofS).
solve_nat(PropS, Proof) :-
  string(PropS),
  !,
  parse_prop(PropS, Prop),
  solve_nat(Prop, Proof).
solve_nat(Prop, pf(Prop, Proof)) :- solve_nat1(Prop, Proof).
solve_nat1(plus_is(z, N, N), 'P-Zero') :- !.
solve_nat1(plus_is(s(N1), N2, s(N3)), 'P-Succ'(Proof1)) :-
  !,
  solve_nat(plus_is(N1, N2, N3), Proof1).
solve_nat1(times_is(z, _, z), 'T-Zero') :- !.
solve_nat1(times_is(s(N1), N2, N4), 'T-Succ'(Proof1, Proof2)) :-
  !,
  solve_nat(times_is(N1, N2, N3), Proof1),
  solve_nat(plus_is(N2, N3, N4), Proof2).
