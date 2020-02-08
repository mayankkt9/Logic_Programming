compress([],[]).
compress([H],[H]).
compress([H,H|T],Result) :- 
	compress([H|T],Result).
compress([H,Y|T],[H|Result]) :-
	H \== Y,
	compress([Y|T],Result).