:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*reflexive-transitive-symmetric closure*/
mem((X,Y), reftransymclos(R,S)) :- mem((X,Y), R), !.
mem((X,X), reftransymclos(R,S)) :- mem(X, S), !.


