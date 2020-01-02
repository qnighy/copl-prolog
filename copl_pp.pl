% vim: ft=prolog :

:- module(copl_pp, [pp_proof/2, pp_prop/2, pp_nat/2]).

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
pp_prop(Prop, S) :-
  term_string(Prop, PropS),
  atomics_to_string(["<cannot pp ", PropS, ">"], S).

pp_nat(z, "Z") :- !.
pp_nat(s(N), S) :-
  !,
  pp_nat(N, NS),
  atomics_to_string(["S(", NS, ")"], S).
pp_nat(N, S) :-
  term_string(N, NS),
  atomics_to_string(["<cannot pp ", NS, ">"], S).
