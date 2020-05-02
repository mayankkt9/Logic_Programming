
:- use_module(library(clpfd)).

%% Question 1 - N Queens 
%% Main Predicate that runs nquees and call helper predicates
queens(N,Qs) :- 
    length(Qs,N),
    Qs ins 1..N,
    all_distinct(Qs),
    safe_queens(Qs),
    labeling([ffc],Qs).

%% This predicate is used to check whether queens are placed with constraint in mind
safe_queens([]).
safe_queens([Q|Qs]) :-
    safe_qn(Qs, Q, 1),
    safe_queens(Qs).

%% This traverses Current queen with all other to attach constraints
safe_qn([],_,_).
safe_qn([Q|Qs],Q0,D0) :-
    abs(Q0-Q) #\= D0,
    D1 #= D0 + 1,
    safe_qn(Qs,Q0,D1).


%% Question 2 Sudoku
%% This is the problem set
problem(1,
        [[_,_,6, 5,9,_, _,_,_], 
         [_,_,3, _,_,_, _,7,_], 
         [_,_,_, _,_,_, 5,6,_], 
         [_,2,_, 1,7,_, _,_,_],
         [4,8,5, _,_,_, _,_,_],
         [_,6,_, _,_,4, 9,_,_],
         [2,_,_, _,_,5, _,_,8],
         [_,3,8, _,_,1, _,_,_],
         [_,_,_, 3,_,_, 7,5,4]]).


%% This predicate is the main sudoku predicate that calls helper predicates
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

%% This set constraints on blocks
blocks([],[],[]).
blocks([A1,A2,A3|A],[B1,B2,B3|B],[C1,C2,C3|C]) :-
    Temp = [A1,A2,A3,B1,B2,B3,C1,C2,C3],
    all_distinct(Temp),
    blocks(A,B,C).

%% This set constraints on rows and columns
setLines([]).
setLines([Row|T]) :-
    length(Row,9),
    Row ins 1..9,
    all_distinct(Row),
    setLines(T).



%% Question 3 Map Coloring


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
connections(X,Y) :-
    edge(X,Y);edge(Y,X). 

%% Color Database
color(1,red).
color(2,blue).
color(3,green).
color(4,yellow).

%% This generate vertex list from all vertex database
generate_vertex_list(L) :-
    findall(X, vertex(X), L), 
    maplist(vertex, L).

%% This is the main map coloring predicate that calls helper predicate
color_map(AnswertoColorlist) :-
    generate_vertex_list(VertexList),
    length(VertexList,VertexCount),
    length(ColorList,VertexCount),
    ColorList ins 1..4,
    generate_answer_list(VertexList,ColorList,Answerlist),
    is_safe(Answerlist),
    label(ColorList),
    colorAt(Answerlist,AnswertoColorlist).

%% This combines vertex and color list this way [[V1,C1],[V2,C2],[V3,C3],[V4,C4],[V5,C5],[V6,C6]]
generate_answer_list([],[],[]).
generate_answer_list([V|T1],[C|T2],[[V,C]|T3]) :-
    generate_answer_list(T1,T2,T3).


%% This checks if it is safe to color vertex
is_safe([]).
is_safe([[V, C]|T]):- check_all(V, C, T), is_safe(T).


% this apply constraints by checking current vertex with all other vertex wether they are adjacent
check_all(_,_, []).
check_all(V1, C1, [[V2, C2]|T]):- connections(V1, V2), C1 #\= C2, check_all(V1, C1, T).
check_all(V1, C1, [[V2,_]|T]):- \+ connections(V1,V2), check_all(V1, C1, T).

%% Map Color number to Color name
colorAt([],[]).
colorAt([[V,C]|T],[[V,N]|T2]) :-
    color(C,N),
    colorAt(T,T2).


%% Question 4 solve zebra

%% This is the main solveZebra predicate that calls helper predicate
solveZebra(Zebra,Water) :-
    Nations = [English, Spanish, Ukrainian, Norwegian, Japanese],
    Colors = [Red, Green, Yellow, Blue, White],
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
    flatten(X,Ans),
    label(Ans).

%% This add constraints if two value are neighbour
next(A,B) :-
    abs(A-B) #= 1.

%% This set constraint for all type of values
set_constraint([]).
set_constraint([H|T]) :-
    H ins 1..5,
    all_distinct(H),
    set_constraint(T).
    
    








    
    
