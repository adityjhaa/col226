:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*reflexive closure*/
mem((X,X), refclos(R,S)) :- mem(X,S), !.
mem((X,Y), refclos(R,S)) :- mem((X,Y), R), !.

/*transitive closure*/
mem((X,Y), tranclos(R)) :- mem((X,Y), R), !.
mem((X,Z), tranclos(R)) :- mem((X,Y), R), mem((Y,Z), tranclos(R)), !.

/*symmetric closure*/
mem((X,Y), symclos(R)) :- mem((X,Y), R), !.
mem((Y,X), symclos(R)) :- mem((X,Y), R), !.

/*reflexive-transitive closure*/
mem((X,Y), reftranclos(R,S)) :- mem((X,Y), refclos(R,S)), !.
mem((X,Y), reftranclos(R,S)) :- mem((X,Y), tranclos(R)), !.

/*reflexive-transitive-symmetric closure*/
mem((X,Y), reftransymclos(R,S)) :- mem((X,Y), refclos(R,S)), !.
mem((X,Y), reftransymclos(R,S)) :- mem((X,Y), tranclos(R)), !.
mem((X,Y), reftransymclos(R,S)) :- mem((X,Y), symclos(R)), !.