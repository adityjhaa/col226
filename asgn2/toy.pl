/*
Interpreter
*/

eval(numeral(N),N) :- integer(N).
eval(plus(E1,E2), N) :- eval(E1, N1), eval(E2, N2), N is N1+N2.
eval(times(E1,E2), N) :- eval(E1, N1), eval(E2, N2), N is N1*N2.

/*
A test case:
eval( plus(times(numeral(3), numeral(4)), times(numeral(5), numeral(6))), V).

V = 42.
*/

/*
Specifications
*/

calculate(numeral(N),numeral(N)) :- integer(N).
calculate(plus(E1,E2), numeral(N)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), N is N1+N2.
calculate(times(E1,E2), numeral(N)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), N is N1*N2.

/*
A test case:
calculate(plus(times(numeral(3), numeral(4)), times(numeral(5), numeral(6))), numeral(N)).

N = 42.
*/

append([], L, L).
append([X|R], L, [X|Z]):- append(R, L, Z).

/*
Compiler
*/

compile(numeral(N),[ldop(N)]) :- integer(N).
compile(plus(E1,E2),C ) :- compile(E1, C1), compile(E2, C2), append(C1, C2, C3), append(C3, [plusop], C). 
compile(times(E1,E2),C ) :- compile(E1, C1), compile(E2, C2), append(C1, C2, C3), append(C3, [timesop], C). 


/*
The Stack Machine
*/

stackmc( [A|S1], [ ], A).
stackmc( S, [ldop(N)|C], A) :- stackmc([N|S], C, A).
stackmc( [N2|[N1|S]], [plusop|C], A ) :- N is N1+N2, stackmc([N|S], C, A).
stackmc( [N2|[N1|S]], [timesop|C], A ) :- N is N1*N2, stackmc([N|S], C, A).


/*
Suggested test query:
compile(plus( times(numeral(3), numeral(4)), times(numeral(5), numeral(6)) ), C),
stackmc( [ ], C, A).

C = [ldop(3), ldop(4), timesop, ldop(5), ldop(6), timesop, plusop],
A = 42 .

*/



