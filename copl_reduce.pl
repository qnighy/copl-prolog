% vim: ft=prolog :

:- module(copl_reduce, [
     solve_reduce_pp/2,
     solve_reduce/2
   ]).
:- use_module([copl_parser, copl_pp, copl_nat]).

solve_reduce_pp(Prop, ProofS) :- solve_reduce(Prop, Proof), pp_proof(Proof, ProofS).
solve_reduce(PropS, Proof) :-
  string(PropS),
  !,
  parse_prop(PropS, Prop),
  solve_reduce(Prop, Proof).
solve_reduce(Prop, pf(Prop, Proof)) :- solve_reduce1(Prop, Proof).
solve_reduce1(reduce(plus(nat(N1), nat(N2)), nat(N3)), 'R-Plus'(Proof1)) :-
  !,
  solve_nat(plus_is(N1, N2, N3), Proof1).
solve_reduce1(reduce(times(nat(N1), nat(N2)), nat(N3)), 'R-Times'(Proof1)) :-
  !,
  solve_nat(times_is(N1, N2, N3), Proof1).
solve_reduce1(reduce(plus(E1, E2), plus(E1a, E2)), 'R-PlusL'(Proof1)) :-
  solve_reduce(reduce(E1, E1a), Proof1),
  !.
solve_reduce1(reduce(plus(E1, E2), plus(E1, E2a)), 'R-PlusR'(Proof1)) :-
  solve_reduce(reduce(E2, E2a), Proof1),
  !.
solve_reduce1(reduce(times(E1, E2), times(E1a, E2)), 'R-TimesL'(Proof1)) :-
  solve_reduce(reduce(E1, E1a), Proof1),
  !.
solve_reduce1(reduce(times(E1, E2), times(E1, E2a)), 'R-TimesR'(Proof1)) :-
  solve_reduce(reduce(E2, E2a), Proof1),
  !.
solve_reduce1(reduce_d(plus(nat(N1), nat(N2)), nat(N3)), 'DR-Plus'(Proof1)) :-
  !,
  solve_nat(plus_is(N1, N2, N3), Proof1).
solve_reduce1(reduce_d(times(nat(N1), nat(N2)), nat(N3)), 'DR-Times'(Proof1)) :-
  !,
  solve_nat(times_is(N1, N2, N3), Proof1).
solve_reduce1(reduce_d(plus(nat(N1), E2), plus(nat(N1), E2a)), 'DR-PlusR'(Proof1)) :-
  !,
  solve_reduce(reduce_d(E2, E2a), Proof1).
solve_reduce1(reduce_d(plus(E1, E2), plus(E1a, E2)), 'DR-PlusL'(Proof1)) :-
  !,
  solve_reduce(reduce_d(E1, E1a), Proof1).
solve_reduce1(reduce_d(times(nat(N1), E2), times(nat(N1), E2a)), 'DR-TimesR'(Proof1)) :-
  !,
  solve_reduce(reduce_d(E2, E2a), Proof1).
solve_reduce1(reduce_d(times(E1, E2), times(E1a, E2)), 'DR-TimesL'(Proof1)) :-
  !,
  solve_reduce(reduce_d(E1, E1a), Proof1).
solve_reduce1(reduce_m(E1, E2), 'MR-One'(Proof1)) :-
  solve_reduce(reduce(E1, E2), Proof1),
  !.
solve_reduce1(reduce_m(E1, E3), 'MR-Multi'(Proof1, Proof2)) :-
  ground(E3),
  solve_reduce(reduce_m(E1, E2), Proof1),
  solve_reduce(reduce_m(E2, E3), Proof2),
  !.
solve_reduce1(reduce_m(E1, E2), 'MR-Zero') :- ground(E2), E1 = E2, !.
