
%:- use_rendering(chess).

queens(N,Qs) :- 
    length(Qs,N),
    Qs ins 1..N,
    all_distinct(Qs),
    safe_queens(Qs),
    labeling([ffc],Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
    safe_qn(Qs, Q, 1),
    safe_queens(Qs).

safe_qn([],_,_).
safe_qn([Q|Qs],Q0,D0) :-
    abs(Q0-Q) #\= D0,
    D1 #= D0 + 1,
    safe_qn(Qs,Q0,D1).


%% :- use_module(library(clpfd)).
%% :- use_rendering(sudoku).

%% problem(1,
%%         [[_,_,6, 5,9,_, _,_,_], 
%%          [_,_,3, _,_,_, _,7,_], 
%%          [_,_,_, _,_,_, 5,6,_], 
%%          [_,2,_, 1,7,_, _,_,_],
%%          [4,8,5, _,_,_, _,_,_],
%%          [_,6,_, _,_,4, 9,_,_],
%%          [2,_,_, _,_,5, _,_,8],
%%          [_,3,8, _,_,1, _,_,_],
%%          [_,_,_, 3,_,_, 7,5,4]]).

sudoku(Rows) :-
    length(Rows,9),
    setLines(Rows),
    transpose(Rows,Columns),
    setLines(Columns),
    Rows = [A,B,C,D,E,F,G,H,I],
    blocks(A,B,C),
    blocks(D,E,F),
    blocks(G,H,I),
    flatten(Rows,Soln),
    labeling([ffc],Soln).

blocks([],[],[]).
blocks([A1,A2,A3|A],[B1,B2,B3|B],[C1,C2,C3|C]) :-
    Temp = [A1,A2,A3,B1,B2,B3,C1,C2,C3],
    all_distinct(Temp),
    blocks(A,B,C).


setLines([]).
setLines([Row|T]) :-
    length(Row,9),
    Row ins 1..9,
    all_distinct(Row),
    setLines(T).

:- use_module(library(clpfd)).

solveZebra(Zebra,Water) :-
    Nations = [English, Spanish, Ukarainian, Norwegian, Japanese],
    Colors = [Red, Green, Yellow, Bule, White],
    Pets = [Dog, Serpent, Fox, Horse, Zebra],
    Drinks = [Coffee, Tea, Milk, Juice, Water],
    Ciggarettes = [Winston, Kool, Chesterfield, LuckyStrike, Kent],
    
    X = [Nations, Colors, Pets, Drinks, Ciggarettes],
    set_constraint(X),
    English #= Red,
    Spanish #= Dog,
    Coffee #= Green,
    Ukrainian #= Tea,
    next(Green,White),
    Winston #= Serpent,
    Yellow #=Kool,
    Milk #= 3,
    Norwegian #= 1,
    next(Chesterfield,Fox),
    next(Horse,Kool),
    LuckyStrike #= Juice,
    Japanese #= Kent,
    next(Norwegian,Blue),
    Flatten(X,Ans),
    label(Ans).

next(A,B) :-
    abs(A-B) #= 1.

set_constraint([]).
set_constraint([H|T]) :-
    H ins 1..5,
    all_distinct(H),
    set_constraint(T).
    
    








:- use_module(library(clpfd)).

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
color(1,red).
color(2,blue).
color(3,green).
color(4,yellow).

generate_vertex_list(L) :-
    findall(X, vertex(X), L), 
    maplist(vertex, L).



color_map(Answerlist) :-
    generate_vertex_list(VertexList),
    length(VertexList,VertexCount),
    length(ColorList,VertexCount),
    ColorList ins 1..4,
    generate_answer_list(VertexList,ColorList,Answerlist),
    is_safe(Answerlist).

generate_answer_list([],[],[]).
generate_answer_list([V|T1],[C|T2],[[V,C]|T3]) :-
    generate_answer_list(T1,T2,T3).

is_safe(Answerlist) :-
    edge(X,Y),
    colorAt(Answerlist,X,CX),
    colorAt(Answerlist,Y,CY),
    CX #\= CY.


colorAt([[V,C]|_],V,C).
colorAt([[V,_]|T],X,CX) :-
           V \= X,
           colorAt(T,X,CX).
    
    
    
