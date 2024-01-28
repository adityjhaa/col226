## Assignment 2

Consider the language of expressions with variables.

E ∈ Exp ::= N ∣ T ∣ F ∣ x ∣ E1 + E2 ∣ E1 * E2 ∣ E1 ∧ E2 ∣ E1 ∨ E2 ∣ ¬E1 ∣ E1 = E2 ∣ E1 > E2 
Extend this language with other arithmetic, boolean and comparison operators and any other kind of expressions.  

Assume types intT and boolT.  You may add other types such as unitT.  

Encode the type-checking relation in PROLOG as a predicate hastype(G,E,T).
