copylist([],[]).
copylist([H|T],[H|Result]) :-
	copylist(T,Result).