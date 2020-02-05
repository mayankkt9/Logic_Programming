flat1([],[]).

flat1([H|T],List) :-
	flat1(H,R1),
	flat1(T,R2),
	append(R1,R2,List).

flat1([H|T],[H|List]) :-
	not(is_list(H)),
	flat1(T,List).