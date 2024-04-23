hasType(num(X), intT) :- !.

hasType(plus(E1,E2), intT) :- hasType(E1, intT), hasType(E2, intT).
hasType(times(E1,E2), intT) :- hasType(E1, intT), hasType(E2, intT).
