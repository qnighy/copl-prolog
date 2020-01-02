% vim: ft=prolog :

:- module(copl_pp, [pp_proof/2]).

pp_proof(Proof, S) :-
  pp_proof(Proof, "", [], Parts),
  atomics_to_string(Parts, S).
pp_proof(pf(Prop, Proof), I, Rem, Parts) :-
  !,
  term_string(Prop, PropS),
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
