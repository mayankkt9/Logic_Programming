pack([], []).
pack([X], [[X]]).

pack([H1, H2|T], [[H1]|TResult]) :-
	H1 \== H2,
	pack([H2|T], TResult).

pack([H, H|T], [[H|TSubResult]|TResult]) :-
	pack([H|T],[TSubResult|TResult]).

encode(List,Result) :-
	pack(List,PList),
	compressL(PList,Result).

compressL([],[]).
compressL([[H|Tsublist]|TList],[[Num|H]|Tail]) :-
	length([H|Tsublist],Num),
	compressL(TList,Tail).