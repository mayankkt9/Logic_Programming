

combination(0,_,[]).
combination(N,List,[H|T]) :-
	N>0,
	select_after(H,List,Res),
	N1 is N-1,
	combination(N1,Res,T).


select_after(H,[H|T],T).
%% Dont append number previous to X
select_after(X,[_|T],Result) :-
	select_after(X,T,Result).