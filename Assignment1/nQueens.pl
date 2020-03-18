
%% Wrapper Predicate that generate row and then calls queens predicate
queens(N,Result) :-
	make_all_row(1,N,Row),
	queens([],Result,Row).

%% This predicate select rows to put queens at, if it finds the safe position 
%% it goes to the next column and try the same thing again, if at anytime queens 
%% attack , it tries different row, and if all column are exhausted it backtracks 
%% to previous column and tries different rows.
queens(ResultSoFar,Result,AllRowList) :-
	select(PickedRow,AllRowList,NotPicked),
	not(not_safe(PickedRow,ResultSoFar)),
	queens([PickedRow|ResultSoFar],Result,NotPicked).

queens(Result,Result,[]).

%% This predicate checks if the current queen placed is not attacked 
%% by the previous queens that are already placed. Counter is starting from 1 because
%% the safe queen list made is in reverse.
not_safe(Row,AllRow) :-
	not_safe(Row,1,AllRow).
not_safe(Row,Counter,[H|_]) :- 
	Row is H + Counter.
not_safe(Row,Counter,[H|_]) :- 
	Row is H - Counter.
not_safe(Row,Counter,[_|T]) :-
	C1 is Counter+1,
	not_safe(Row,C1,T).

%% This predicate is used to generate row lists
make_all_row(End,End,[End]).
make_all_row(Start,End,[Start|Tail]) :-
	Start < End,
	NextRow is Start+1,
	make_all_row(NextRow,End,Tail).

