:- use_module(library(clpfd)).
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
    Nations = [Englist, Spanish, Ukarainian, Norwegian, Japanese],
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
    
    
