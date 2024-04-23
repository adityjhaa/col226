likes(mike, cake).
likes(tom, cake).

friends(X,Y) :- likes(X, cake), likes(Y, cake).