:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*transitive closure*/
mem((X,Y), tc(R, S)):- mem((X,Y), R),!.
mem((X,Y), tc(R, S)):- mem((X,Z), R), mem((Z,Y), tc(R, S))!.
