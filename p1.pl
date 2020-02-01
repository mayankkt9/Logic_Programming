lastelement(X,[X]).
lastelement(X,[_|T]) :- lastelement(X,T).