element_at(X,[X|_],1).
element_at(X,[_|T],K) :- NK is K-1,element_at(X,T,NK).