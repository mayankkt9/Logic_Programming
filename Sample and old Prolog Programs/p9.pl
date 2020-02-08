pack([], []).
pack([X], [[X]]).

pack([H1, H2|T], [[H1]|TResult]) :-
	H1 \== H2,
	pack([H2|T], TResult).

pack([H, H|T], [[H|TSubResult]|TResult]) :-
	pack([H|T],[TSubResult|TResult]).