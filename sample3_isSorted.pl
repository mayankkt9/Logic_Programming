sorted([]).
sorted([_]).
sorted([A,B|T]) :- A=<B, sorted([B|T]).
