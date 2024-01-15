:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*reflexive-transitive closure*/
mem((X,Y), reftranclos(R,S)) :- mem((X,Y), R), !.
mem((X,X), reftranclos(R,S)) :- mem(X, S), !.
mem((X,Y), reftranclos(R,S)) :- mem((X,Z), R), mem((Z,Y), reftranclos(R,S)), !.


