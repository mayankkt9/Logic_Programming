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


do_coloring(_) :-
	vertex(X),
	write(X).

color_map(L) :-
	do_coloring(L).
	
