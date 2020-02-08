%% ?- hanoi(3,a,c,b).

hanoi(1,A,B,_) :-
	write('Move '),
	write(A),
	write(' '),
	write('to '),
	write(B),
	nl.

hanoi(N,A,C,B) :-
	N>1,
	N1 is N-1,
	hanoi(N1,A,B,C),
	hanoi(1,A,C,_),
	hanoi(N1,B,C,A).