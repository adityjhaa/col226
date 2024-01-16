:- use_module(library(lists)).

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

diff(S1,[],S1) :- !.
diff([],S2,[]) :- !.
diff([X|L],S2,[X|Z]) :- \+ mem(X,S2), diff(L,S2,Z), !.
diff([_|L],S2,Z) :- diff(L,S2,Z).
