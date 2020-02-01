secondlast(X,[X|[_]]).
secondlast(X,[H|T]) :- secondlast(X,T).