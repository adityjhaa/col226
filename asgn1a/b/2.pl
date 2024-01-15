del(X,[],[]):- !.
del(X,[X|R],Z):- del(X,R,Z), !.
del(X,[Y|R],[Y|Z]):- del(X,R,Z), !.

remdup([],[]):- !.
remdup([X|R],[X|Z]):- del(X,R,L), remdup(L,Z).

unionI(S1,[],S1):- !.
unionI([],S2,S2):- !.
unionI([X|R],S2,[X|Z]):- del(X,S2,S3), unionI(R,S3,Z).

/*
Examples to check that union doesn't have duplicates.
    
    Input : unionI([1,2,3,4,5],[4,3,2,1],U).
    Output : U = [1, 2, 3, 4, 5].
    This function has neglected the duplicates 1,2,3,4.

    Input : unionI([1,2,4,9,10,203,90],[90,702,2048,42],U).
    Output : U = [1, 2, 4, 9, 10, 203, 90, 702, 2048, 42].
    Did not duplicate 90.

    Input : unionI([a,2,q,10], [2,a],U).
    Output : U = [a, 2, q, 10].
    a, 2 are not taken twice.

These show that the union doesn't have duplicates.
*/