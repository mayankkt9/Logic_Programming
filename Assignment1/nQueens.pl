queens(N,Result) :-
	make_all_row(1,N,Row),
	queens([],Result,Row).

queens(ResultSoFar,Result,AllRowList) :-
	select(PickedRow,AllRowList,NotPicked),
	not(not_safe(PickedRow,ResultSoFar)),
	queens([PickedRow|ResultSoFar],Result,NotPicked).

queens(Result,Result,[]).

not_safe(Row,AllRow) :-
	not_safe(Row,1,AllRow).

not_safe(Row,Counter,[H|_]) :-
	Row is H + Counter;
	Row is H - Counter.

not_safe(Row,Counter,[_|T]) :-
	C1 is Counter+1,
	not_safe(Row,C1,T).


make_all_row(End,End,[End]).
make_all_row(Start,End,[Start|Tail]) :-
	Start < End,
	NextRow is Start+1,
	make_all_row(NextRow,End,Tail).

