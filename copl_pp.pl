% vim: ft=prolog :

:- module(copl_pp, [pp_proof/2, pp_prop/2, pp_exp/2, pp_nat/2]).

pp_proof(Proof, S) :-
  pp_proof(Proof, "", [], Parts),
  atomics_to_string(Parts, S).
pp_proof(pf(Prop, Proof), I, Rem, Parts) :-
  !,
  pp_prop(Prop, PropS),
  Parts = [PropS, " by "|Rem1],
  pp_proof1(Proof, I, Rem, Rem1).
pp_proof(Proof, _, Rem, Parts) :-
  term_string(Proof, ProofS),
  Parts = ["<cannot pp ", ProofS, ">"|Rem].

pp_proof1(Proof, _, Rem, Parts) :-
  atomic(Proof),
  !,
  Parts = [Proof, " {}"|Rem].
pp_proof1(Proof, I, Rem, Parts) :-
  compound(Proof),
  !,
  compound_name_arguments(Proof, ProofName, ProofArgs),
  string_concat(I, "  ", I2),
  Parts = [ProofName, " {\n"|Rem1],
  pp_proof1c(ProofArgs, _, I2, [I, "}"|Rem], Rem1).
pp_proof1(Proof, _, Rem, Parts) :-
  term_string(Proof, ProofS),
  Parts = ["<cannot pp ", ProofS, ">"|Rem].

pp_proof1c([], "", _, Rem, Rem) :- !.
pp_proof1c([ArgH|ArgT], ";", I, Rem, [I|Parts]) :-
  !,
  pp_proof(ArgH, I, [Sep, "\n"|Rem1], Parts),
  pp_proof1c(ArgT, Sep, I, Rem, Rem1).

pp_prop(plus_is(N1, N2, N3), S) :-
  !,
  pp_nat(N1, N1S),
  pp_nat(N2, N2S),
  pp_nat(N3, N3S),
  atomics_to_string([N1S, " plus ", N2S, " is ", N3S], S).
pp_prop(times_is(N1, N2, N3), S) :-
  !,
  pp_nat(N1, N1S),
  pp_nat(N2, N2S),
  pp_nat(N3, N3S),
  atomics_to_string([N1S, " times ", N2S, " is ", N3S], S).
pp_prop(is_less_than(N1, N2), S) :-
  !,
  pp_nat(N1, N1S),
  pp_nat(N2, N2S),
  atomics_to_string([N1S, " is less than ", N2S], S).
pp_prop(evalto(E, N), S) :-
  !,
  pp_exp(E, ES),
  pp_nat(N, NS),
  atomics_to_string([ES, " evalto ", NS], S).
pp_prop(reduce(E1, E2), S) :-
  !,
  pp_exp(E1, E1S),
  pp_exp(E2, E2S),
  atomics_to_string([E1S, " ---> ", E2S], S).
pp_prop(reduce_d(E1, E2), S) :-
  !,
  pp_exp(E1, E1S),
  pp_exp(E2, E2S),
  atomics_to_string([E1S, " -d-> ", E2S], S).
pp_prop(reduce_m(E1, E2), S) :-
  !,
  pp_exp(E1, E1S),
  pp_exp(E2, E2S),
  atomics_to_string([E1S, " -*-> ", E2S], S).
pp_prop(Prop, S) :-
  term_string(Prop, PropS),
  atomics_to_string(["<cannot pp ", PropS, ">"], S).

pp_exp(E, S) :-
  (E = plus(_, _); E = times(_, _); E = nat(_)),
  !,
  pp_exp_plus(E, S).
pp_exp(E, S) :-
  term_string(E, ES),
  atomics_to_string(["<cannot pp ", ES, ">"], S).
pp_exp_plus(plus(E1, E2), S) :-
  !,
  pp_exp_plus(E1, S1),
  pp_exp_times(E2, S2),
  atomics_to_string([S1, " + ", S2], S).
pp_exp_plus(E, S) :- pp_exp_times(E, S).
pp_exp_times(times(E1, E2), S) :-
  !,
  pp_exp_times(E1, S1),
  pp_exp_atom(E2, S2),
  atomics_to_string([S1, " * ", S2], S).
pp_exp_times(E, S) :- pp_exp_atom(E, S).
pp_exp_atom(nat(N), S) :- !, pp_nat(N, S).
pp_exp_atom(E1, S) :-
  pp_exp(E1, S1),
  atomics_to_string(["(", S1, ")"], S).

pp_nat(z, "Z") :- !.
pp_nat(s(N), S) :-
  !,
  pp_nat(N, NS),
  atomics_to_string(["S(", NS, ")"], S).
pp_nat(N, S) :-
  term_string(N, NS),
  atomics_to_string(["<cannot pp ", NS, ">"], S).
