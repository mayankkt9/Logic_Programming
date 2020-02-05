palindrom(L) :- 
	reverseList(L,RevL,[]),
	check(L,RevL).

reverseList([],L,L).
reverseList([H|T],RevL,ACC) :- 
	reverseList(T,RevL,[H|ACC]).

check([],[]).
check([H|T1],[H|T2]) :- 
	check(T1,T2).