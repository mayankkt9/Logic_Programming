numofelement(0,[]).
numofelement(X,[H|T]) :- numofelement(Y,T),X is Y+1.