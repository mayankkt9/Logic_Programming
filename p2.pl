secondlast(X,[X|[_]]).
secondlast(X,[_|T]) :- secondlast(X,T).