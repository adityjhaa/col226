/*----------------------A----------------------*/
/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*-------1-------*/
mem((X,Y), reftranclos(R,S)) :- mem((X,Y), R), !.
mem((X,X), reftranclos(R,S)) :- mem(X, S), !.
mem((X,Y), reftranclos(R,S)) :- mem((X,Z), R), mem((Z,Y), reftranclos(R,S)), !.
mem((X,Y), reftranclos(R,S)) :- mem((Z,Y), R), mem((X,Z), reftranclos(R,S)), !.

/*-------2-------*/
mem((X,Y), reftransymclos(R,S)) :- mem((X,Y), R), !.
mem((X,X), reftransymclos(R,S)) :- mem(X, S), !.
mem((X,Y), reftransymclos(R,S)) :- mem((Y,X), R), !.
mem((X,Y), reftransymclos(R,S)) :- mem((X,Z), R), mem((Z,Y), reftransymclos(R,S)), !.
mem((X,Y), reftransymclos(R,S)) :- mem((Y,X), reftransymclos(R,S)), !.
mem((X,Y), reftransymclos(R,S)) :- mem((Z,Y), R), mem((X,Z), reftransymclos(R,S)), !.

/*----------------------B----------------------*/

del(X,[],[]):- !.
del(X,[X|R],Z):- del(X,R,Z), !.
del(X,[Y|R],[Y|Z]):- del(X,R,Z), !.

remdup([],[]):- !.
remdup([X|R],[X|Z]):- del(X,R,L), remdup(L,Z).

unionI(S1,[],S1):- !.
unionI([],S2,S2):- !.
unionI([X|R],S2,[X|Z]):- del(X,S2,S3), unionI(R,S3,Z).

append([],L,L):- !.
append([X|R],L,[X|Z]):- append(R,L,Z).

mapcons(X,[],[]):- !.
mapcons(X,[Y|R],[[X|Y]|Z]):- mapcons(X,R,Z).

powerI([],[[]]):- !.
powerI([X|R],P):- powerI(R,P1), mapcons(X,P1,P2), append(P2,P1,P).

/*-------1-------*/
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

*/

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

*/

/*-------2-------*/
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

*/

/*-------3-------*/
/*Intersection*/
interI(S1,[],[]) :- !.
interI([],S2,[]) :- !.
interI([X|L],S2,[X|Z]) :- mem(X,S2), interI(L,S2,Z), !.
interI([_|L],S2,Z) :- interI(L,S2,Z).

/*-------4-------*/
/*Set Difference*/
diffI(S1,[],S1) :- !.
diffI([],S2,[]) :- !.
diffI([X|L],S2,[X|Z]) :- \+ mem(X,S2), diffI(L,S2,Z), !.
diffI([_|L],S2,Z) :- diffI(L,S2,Z).

/*-------5-------*/
/*Cartesian Product*/
cartesianI([],S2,[]) :- !.
cartesianI(S1,[],[]) :- !.
cartesianI([X|L],[Y|R],[(X,Y)|Z]) :- cartesianI(L,[Y|R],Z1), cartesianI([X|L],R,Z2), unionI(Z1,Z2,Z).

/*-------6-------*/
/*
Examples to check if the intersection implementation is correct.

    Input : interI([1,2,3],[],I).
    Output : I = [].

    Input : interI([1,2,3],[1],I).
    Output : I = [1].

    Input : interI([1,3,8],[1,10,8],I).
    Output : I = [1, 8].

    Input : interI([a,1,12,c,42],[a,3,42],I).
    Output : I = [a, 42].

    Input : interI([1,3,5,2,4],[6,1,2],I).
    Output : I = [1, 2].

*/


/*
Examples to check if the set-difference implementation is correct.

    Input : diffI([],[1,2,3,4],D).
    Output : D = [].

    Input : diffI([1,2,3,4],[],D).
    Output : D = [1, 2, 3, 4].

    Input : diffI([1,2,3,4],[1,3],D).
    Output : D = [2, 4].

    Input : diffI([1,2,3,4],[1,2,3,4],D).
    Output : D = [].

    Input : diffI([1,2,3,d,42],[1,2,3,4,d],D).
    Output : D = [42].

*/

/*
Examples to check if the cartesian product implementation is correct.

    Input : cartesianI([1,2,3,4],[],C).
    Output : C = [].

    Input : cartesianI([1,2,3,4],[5],C).
    Output : C = [(1, 5),  (2, 5),  (3, 5),  (4, 5)].

    Input : cartesianI([1],[2,3,4,5],C).
    Output : C = [(1, 2),  (1, 3),  (1, 4),  (1, 5)].

    Input : cartesianI([1,2,3,4],[1,2,3,4,5],C).
    Output : C = [(1, 1),  (2, 1),  (3, 1),  (4, 1),  (4, 2),  (4, 3),  (4, 4),  (4, 5),  (3, 2),  (3, 3),  (3, 4),  (3, 5),  (2, 2),  (2, 3),  (2, 4),  (2, 5),  (1, 2),  (1, 3),  (1, 4),  (1, 5)] .

    Input : cartesianI([1,2,3,4],[a,b,c],C).
    Output : C = [(1, a),  (2, a),  (3, a),  (4, a),  (4, b),  (4, c),  (3, b),  (3, c),  (2, b),  (2, c),  (1, b),  (1, c)].

*/

/*-------7-------*/
/*subset and equal set implementation*/
subset([],S) :- !.
subset([X|L],S2) :- mem(X,S2), subset(L,S2).
eqset(S1,S2) :- subset(S1,S2), subset(S2,S1).
/*mem2 is to check if a set is a member or set of sets*/
mem2(L,[]) :- fail.
mem2(L,[A|S]) :- eqset(L,A), !.
mem2(L,[_|S]) :- mem2(L,S).
/*subset2 is to check if a set of set is a subset of another set of sets*/
subset2([],L) :- !.
subset2([X|L], S2) :- mem2(X, S2), subset2(L,S2).
/*areequalpower is to check if two powersets are identical*/
areequalpower(P1,P2) :- subset2(P1,P2), subset2(P2,P1).
/*
P1 and P2 are the powersets of sets S1 and S2. We get these using the powerI implementation. We use the eqset to check if 2 sets are equal. This helps in the function mem2 to check if a set is a part of a set of sets. This helps define the subset2 function which checks if a set of sets is a subset to another set of sets. This is used to define if two sets of sets are equal.
P1 = powerI(S1), P2 = powerI(S2).
If S1 and S2 are the same and only the ordering is different, then they should produce the same powerset. The areequalpower function checks this case and returns true. But if sets S1 and S2 are different, then the powersets are different and returns false.
*/

