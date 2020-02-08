visitedList(X,[X|_]).
visitedList(X,[_|T]) :- visitedList(X,T).

edge(a,b,1).
edge(b,e,1).
edge(d,e,1).
edge(d,b,3).
edge(c,d,1).
edge(b,c,4).
edge(a,c,6).

connection(X,Y) :-
	edge(X,Y,_);edge(Y,X,_).

findpath(Source,Destination,_,P) :-
	findpath(Source,Destination,[Source],_,Q),
	reverse(Q,P).
findpath(Source,Destination,Visited,_,[Destination|Visited]) :-
	connection(Source,Destination).
findpath(Source,Destination,Visited,Weight,Path) :-
	connection(Source,Vertex),
	Vertex\==Destination,
	not(visitedList(Vertex,Visited)),
	findpath(Vertex,Destination,[Vertex|Visited],Weight,Path).