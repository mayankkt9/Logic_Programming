%% Practice Problem and Understand

%% Sample 1 - member
%% Sample 2 - sorted
%% Sample 3 - list_length
%% Sample 4 - copy_list

%% Program 1 - last list last_list_element
%% Program 2 - list_last_but_one
%% Program 3 - List list_kth_element


%% ------------------------------

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

%% member(6,[1,2,3,4,5]).
%% ------------------------------


sorted([]).
sorted([_]).
sorted([A,B|T]) :-
    A=<B,
    sorted([B|T]).

%% sorted([1,2,3,10,4,5]).
%% ------------------------------

list_length([],0).
list_length([_|T],N) :-
    list_length(T,N1),
   	N is N1 + 1.

list_length_tail(L,N) :- list_length_TL(L,0,N).
list_length_TL([],N,N).
list_length_TL([_|T],Current,N) :-
    Next is Current + 1,
    list_length_TL(T,Next,N).

%% list_length_tail([1,2,3,4,5,19],X).
%% ------------------------------

copy_list([],[]).
copy_list([H|T],[H|Res]) :-
    copy_list(T,Res).

%% copy_list([1,2,3,4,5],X).
%% ------------------------------

last_list_element([X],X).
last_list_element([_|T],Ans) :-
    last_list_element(T,Ans).
%% last_list_element([1,2,3,11,12,14],X).
%% ------------------------------

list_last_but_one([X,Y],X).
list_last_but_one([_|T],Ans) :-
    list_last_but_one(T,Ans).

%% list_last_but_one([1,2,3,11,12,14,11],X).
%% ------------------------------


list_kth_element(X,[X|_],1).
list_kth_element(X,[_|T],K) :-
    K1 is K - 1,
    list_kth_element(X,T,K1).

%% list_kth_element(X,[1,2,3,11,12,14,11],6).
%% ------------------------------

