
%% Stopping condition when N is 0
combination(0,_,[]).

%% Combination predicate that selects each number from the 
%% list such that on the next call previous to the selected 
%% number is not available
combination(N,List,[H|T]) :-
	N>0,
	select_after(H,List,Res),
	N1 is N-1,
	combination(N1,Res,T).


%% When head matches return tail
select_after(H,[H|T],T).
%% Dont append number previous to X
select_after(X,[_|T],Result) :-
	select_after(X,T,Result).