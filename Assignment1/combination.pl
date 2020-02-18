
combination(0,_,[]).
combination(K,List,[H|Tail]) :-
	K > 0,
	helpercomb(H,List,R), 
	K1 is K-1, 
	combination(K1,R,Tail).

helpercomb(H,[H|Tail],Tail).
helpercomb(H,[_|Tail],Result) :- 
	helpercomb(H,Tail,Result).