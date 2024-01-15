:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

interI(S1,[],[]) :- !.
interI([],S2,[]) :- !.
interI([X|L],S2,[X|Z]) :- mem(X,S2), interI(L,S2,Z), !.
interI([_|L],S2,Z) :- interI(L,S2,Z).

