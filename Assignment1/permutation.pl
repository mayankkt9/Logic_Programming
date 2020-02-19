perm([],[]).
perm(L,[X|R]) :-
	select(X,L,RL),
	perm(RL,R).

permutaionN(0,_,[]).
permutaionN(N,L,[H|T]) :-
	N>0,
	select(H,L,R),
	N1 is N-1,
	permutaionN(N1,R,T).
