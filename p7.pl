flat1([H|T],R) :-
	flat1(H,R1),
	flat1(T,R2),
	append(R1,R2,R).