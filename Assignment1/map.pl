vertex(1).
vertex(2).
vertex(3).
vertex(4).
vertex(5).
vertex(6).


edge(1,2).
edge(1,3).
edge(1,6).
edge(1,4).
edge(2,5).
edge(2,3).
edge(3,4).
edge(6,4).
edge(5,4).
edge(5,3).
edge(6,3).

connection(X,Y) :-
	edge(X,Y);edge(Y,X). 

color(red).
color(blue).
color(green).
color(yellow).

color_map(L) :-
	color_map(L,[]).

color_map(Result,ResultSoFar) :-
	vertex(V),
	not(member_list(V,ResultSoFar)),
	color(C),
	is_safe(V,C,ResultSoFar),
	append(ResultSoFar,[[V,C]],ResultTemp),
	color_map(Result,ResultTemp).
color_map(Result,ResultSoFar) :-
	aggregate_all(count, vertex(X), Count),
	length(ResultSoFar,CurrentCount),
	Count == CurrentCount,
	Result = ResultSoFar.

member_list(V,[[V,_]|_]).
member_list(V,[[_,_]|Tail]) :-
	member_list(V,Tail).

is_safe(_,_,[]).
is_safe(_,_,[[]]).
is_safe(Vertex,Color,[[V,C]|Tail]) :-
	connection(Vertex,V),
	Color \= C,
	is_safe(Vertex,Color,Tail).

is_safe(Vertex,Color,[[V,_]|Tail]) :-
	not(connection(Vertex,V)),
	is_safe(Vertex,Color,Tail).

	






