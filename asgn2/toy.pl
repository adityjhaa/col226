
/*
Specifications
*/

calculate(numeral(N),numeral(N)) :- integer(N).
calculate(plus(E1,E2), numeral(N)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), N is N1+N2.
calculate(times(E1,E2), numeral(N)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), N is N1*N2.

/*
A test case:- 
calculate(plus(times(numeral(3), numeral(4)), times(numeral(5), numeral(6))), numeral(N)).

N = 42.
*/

append([], L, L).
append([X|R], L, [X|Z]):- append(R, L, Z).


