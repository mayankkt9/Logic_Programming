%% Vertex Database
vertex(1).
vertex(2).
vertex(3).
vertex(4).
vertex(5).
vertex(6).

%% Edge Database
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

%% Undirected Edge statisfies this rule 
connection(X,Y) :-
	edge(X,Y);edge(Y,X). 

%% Color Database
color(red).
color(blue).
color(green).
color(yellow).

%% Wrapper predicate
color_map(L) :-
	generate_vertex_list(VertexList),
	color_map(L,[],VertexList).

%% This predicate does all the work, select vertex one by one 
%% and applies all color to it if it is safe
color_map(Result,ResultSoFar,VertexList) :-
	select_after(V,VertexList,RemainList),
	not(member_list(V,ResultSoFar)),
	color(C),
	is_safe(V,C,ResultSoFar),
	append(ResultSoFar,[[V,C]],ResultTemp),
	color_map(Result,ResultTemp,RemainList).

%% This predicate is the stopping condition when the length 
%% of the solution is equal to the number of vertices
color_map(Result,ResultSoFar,_) :-
	aggregate_all(count, vertex(_), Count),
	length(ResultSoFar,CurrentCount),
	Count == CurrentCount,
	Result = ResultSoFar.

%% Predicate to select if the vertex is not in the visited list
member_list(V,[[V,_]|_]).
member_list(V,[[_,_]|Tail]) :-
	member_list(V,Tail).

%% Checking whether giving colors attacks or not
is_safe(_,_,[]).
is_safe(Vertex,Color,[[V,C]|Tail]) :-
	connection(Vertex,V),
	Color \= C,
	is_safe(Vertex,Color,Tail).

is_safe(Vertex,Color,[[V,_]|Tail]) :-
	not(connection(Vertex,V)),
	is_safe(Vertex,Color,Tail).

%% https://stackoverflow.com/questions/13431407/how-to-get-a-list-of-possible-predicate-values
generate_vertex_list(L) :-
	findall(X, vertex(X), L), 
	maplist(vertex, L).
	

select_after(H,[H|T],T).
%% Dont append number previous to X
select_after(X,[_|T],Result) :-
	select_after(X,T,Result).




