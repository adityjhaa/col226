/*membership*/
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R), !.


hastype(G,intT(N),intT):- !.
hastype(G,boolT(B),boolT):- !.
hastype(G,varT(X),intT):- mem((X,intT),G).
hastype(G,varT(X),boolT):- mem((X,boolT),G).
hastype(G,add(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,subt(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,mul(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,divt(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,max(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,min(E1,E2),intT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,gtn(E1,E2),boolT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,ltn(E1,E2),boolT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,eql(E1,E2),boolT):- hastype(G,E1,intT), hastype(G,E2,intT).
hastype(G,myand(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).
hastype(G,myor(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).
hastype(G,mynot(E1),boolT):- hastype(G,E1,boolT).
hastype(G,mynor(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).
hastype(G,mynand(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).
hastype(G,myxor(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).
hastype(G,myxnor(E1,E2),boolT):- hastype(G,E1,boolT), hastype(G,E2,boolT).




