numofelement(0,[]).
numofelement(X,[_|T]) :- numofelement(Y,T),X is Y+1.