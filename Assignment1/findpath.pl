%% Find a path in a weighted graph:
%% This program is written with an assumption
%%  - Source and Destination is not same (This can be easily handled)

%% Edge database to store undirected edge
edge(a,b,1).
edge(b,e,1).
edge(d,e,1).
edge(d,b,3).
edge(c,d,1).
edge(b,c,4).
edge(a,c,6).

%% As it is an undirected edge this rule statisfies
connection(X,Y,Weight) :-
	edge(X,Y,Weight);edge(Y,X,Weight).

%% Wrapper predicate to call findpath, as we calculated the reversed path, we need to call reverse.
findpath(Source,Destination,Weight,Path) :-
	findpath(Source,Destination,[Source],Weight,RevPath),
	reverse(RevPath,Path),
	weight(Path,Weight).

%% If there is a connection between current source and final destination, 
%% append  the destination to the visited list to get the final path.
findpath(Source,Destination,Visited,_,[Destination|Visited]) :-
	connection(Source,Destination,_).

%% Form the visited list till it finds the destination, so this predicate will 
%% calculate all path to final destination
findpath(Source,Destination,Visited,Weight,Path) :-
	connection(Source,Vertex,_),
	Vertex\==Destination,
	not(visitedList(Vertex,Visited)),
	findpath(Vertex,Destination,[Vertex|Visited],Weight,Path).
	
%% Implementing if vertex X is already visited to avoid cycles
visitedList(X,[X|_]).
visitedList(X,[_|T]) :- visitedList(X,T).

%% When we have got the path from source to destination, 
%% this predicate is traversing the path and calculating total weight
weight([_|[]],0).
weight([H1,H2|T],Weight) :-
	connection(H1,H2,W),
	weight([H2|T],Rest),
	Weight is W+Rest.
