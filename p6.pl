palindrom(L) :- reverse(L,RevL,[]),write(RevL),check(L,RevL).
reverse([],L,L).
reverse([H|T],RevL,ACC) :- reverse(T,RevL,[H|ACC]).
check([],[]).
check([H|T1],[H|T2]) :- check(T1,T2).