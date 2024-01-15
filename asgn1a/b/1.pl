del(X,[],[]):- !.
del(X,[X|R],Z):- del(X,R,Z), !.
del(X,[Y|R],[Y|Z]):- del(X,R,Z), !.

remdup([],[]):- !.
remdup([X|R],[X|Z]):- del(X,R,L), remdup(L,Z).

unionI(S1,[],S1):- !.
unionI([],S2,S2):- !.
unionI([X|R],S2,[X|Z]):- del(X,S2,S3), unionI(R,S3,Z).

/*
Examples for unionI:

    Input : unionI([],[],U).
    Output : U = [].

    Input : unionI([1,2],[1,9],U).
    Output : U = [1, 2, 9].

    Input : unionI([2,1,9],[],U).
    Output : U = [2, 1, 9].

    Input : unionI([],[2,1,9],U).
    Output : U = [2, 1, 9].

    Input : unionI([9,1,7,4,0],[0,1,9,4,7],U).
    Output : U = [9, 1, 7, 4, 0].

All these U are indeed unions of the given sets.
*/

append([],L,L):- !.
append([X|R],L,[X|Z]):- append(R,L,Z).

mapcons(X,[],[]):- !.
mapcons(X,[Y|R],[[X|Y]|Z]):- mapcons(X,R,Z).

powerI([],[[]]):- !.
powerI([X|R],P):- powerI(R,P1), mapcons(X,P1,P2), append(P2,P1,P).

/*
Examples for powerI:

    Input : powerI([],P).
    Output: P = [[]].

    Input : powerI([1],P).
    Output: P = [[1], []].

    Input : powerI([1,2,3,4],P).
    Output: P = [[1, 2, 3, 4], [1, 2, 3], [1, 2, 4], [1, 2], [1, 3, 4], [1, 3], [1, 4], [1], [2, 3, 4], [2, 3], [2, 4], [2], [3, 4], [3], [4], []].

    Input : powerI([[],1,2],P).
    Output: P = [[[], 1, 2], [[], 1], [[], 2], [[]], [1, 2], [1], [2], []].

    Input : powerI([[0],1,2],P).
    Output: P = [[[0], 1, 2], [[0], 1], [[0], 2], [[0]], [1, 2], [1], [2], []].

All these P are indeed powersets of the given set. 
*/
