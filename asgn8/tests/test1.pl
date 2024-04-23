// The member function

mem(X, []) :- fail.
mem(X, [X|_]) :- !.
mem(X, [_|R]) :- mem(X, R).


