reverse(X,X,[]).
reverse(X,ACC,[H|T]) :- reverse(X,[H|ACC],T).