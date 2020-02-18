edge(a,b,1).
edge(b,e,1).
edge(d,e,1).
edge(d,b,3).
edge(c,d,1).
edge(b,c,4).
edge(a,c,6).

connection(X,Y,Weight) :-
	edge(X,Y,Weight);edge(Y,X,Weight).


findpath(Source,Destination,Weight,Path) :-
	findpath(Source,Destination,[Source],Weight,RevPath),
	reverse(RevPath,Path),
	weight(Path,Weight).
findpath(Source,Destination,Visited,_,[Destination|Visited]) :-
	connection(Source,Destination,_).
findpath(Source,Destination,Visited,Weight,Path) :-
	connection(Source,Vertex,_),
	Vertex\==Destination,
	not(visitedList(Vertex,Visited)),
	findpath(Vertex,Destination,[Vertex|Visited],Weight,Path).
	

visitedList(X,[X|_]).
visitedList(X,[_|T]) :- visitedList(X,T).


weight([_|[]],0).
weight([H1,H2|T],Weight) :-
	connection(H1,H2,W),
	weight([H2|T],Rest),
	Weight is W+Rest.
