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
    Input : unionI([1,2],[1,9],P).
    Output : P = [1, 2, 9].

    Input : unionI([2,1,9],[],P).
    Output : P = [2, 1, 9].

    Input : unionI([],[2,1,9],P).
    Output : P = [2, 1, 9].


*/

append([],L,L):- !.
append([X|R],L,[X|Z]):- append(R,L,Z).

mapcons(X,[],[]):- !.
mapcons(X,[Y|R],[[X|Y]|Z]):- mapcons(X,R,Z).

powerI([],[[]]):- !.
powerI([X|R],P):- powerI(R,P1), mapcons(X,P1,P2), append(P2,P1,P).

/*
Examples for powerI:
    


*/