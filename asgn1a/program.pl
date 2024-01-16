/*----------------------A----------------------*/



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

/*1*/
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

/*2*/
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

/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.

/*3*/
/*Intersection*/
interI(S1,[],[]) :- !.
interI([],S2,[]) :- !.
interI([X|L],S2,[X|Z]) :- mem(X,S2), interI(L,S2,Z), !.
interI([_|L],S2,Z) :- interI(L,S2,Z).

/*4*/
/*Set Difference*/
diffI(S1,[],S1) :- !.
diffI([],S2,[]) :- !.
diffI([X|L],S2,[X|Z]) :- \+ mem(X,S2), diffI(L,S2,Z), !.
diffI([_|L],S2,Z) :- diffI(L,S2,Z).

/*5*/
/*Cartesian Product*/
cartesianI([],S2,[]) :- !.
cartesianI(S1,[],[]) :- !.
cartesianI([X|L],[Y|R],[(X,Y)|Z]) :- cartesianI(L,[Y|R],Z1), cartesianI([X|L],R,Z2), unionI(Z1,Z2,Z).

/*6*/
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

Surely, this implementation finds the intersection of two sets without faults.
*/


/*
Examples to check if the set-difference implementation is correct.

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 


*/

/*
Examples to check if the cartesian product implementation is correct.

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 

    Input : 
    Output : 


*/

/*7*/
