%% Practice Problem and Understand

%% Sample 1 - member
%% Sample 2 - sorted
%% Sample 3 - list_length
%% Sample 4 - copy_list

%% Program 1 - last list last_list_element
%% Program 2 - list_last_but_one
%% Program 3 - List list_kth_element
%% Program 4 - reverse_list
%% Program 5 - is_palindrome
%% Program 6 - flat_list
%% Program 7 - eliminate_consecutive
%% Program 8 - pack_same_element
%% Program 9 - encode
%% Program 10 - encode

%% TODO
%% gcd
%% append
%% permutaion
%% list_length

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


reverse_list([],Ans,Ans).
reverse_list([H|T],Acc,Ans) :-
    reverse_list(T,[H|Acc],Ans).

%% reverse_list([1,2,3,11,12,14,11],[],Ans).
%% ------------------------------

is_palindrome([]).
is_palindrome(L) :-
    reverse_list(L,[],RevList),
    is_same(L,RevList).

is_same([],[]).
is_same([H1|T1],[H2|T2]) :-
    H1 = H2,
    is_same(T1,T2).


reverse_list([],Ans,Ans).
reverse_list([H|T],Acc,Ans) :-
    reverse_list(T,[H|Acc],Ans).


%% is_palindrome([1,2,3,4,3,2,1,2]).
%% ------------------------------

flat_list([],[]).

flat_list([H|T],[H|List]) :-
    not(is_list(H)),
    flat_list(T,List).

flat_list([H|T],List):-
    flat_list(H,R1),
    flat_list(T,R2),
    append(R1,R2,List).

%% flat_list([1,2,3,4,3,2,1,2,[1,2]],X).
%% ------------------------------


eliminate_consecutive([],[]).
eliminate_consecutive([A],[A]).
eliminate_consecutive([H1,H2|T],[H1|List]) :- 
    H1 \= H2,
    eliminate_consecutive([H2|T],List).
eliminate_consecutive([H,H|T],List) :- 
    eliminate_consecutive([H|T],List).

%% eliminate_consecutive([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% ------------------------------


pack_same_element([],[]).
pack_same_element([A],[[A]]).
pack_same_element([H1,H2|T],[[H1]|Tail]) :-
    H1 \= H2,
 	pack_same_element([H2|T],Tail).

pack_same_element([H,H|T],[[H|SubList]|List]) :-
    pack_same_element([H|T],[SubList|List]).

%% pack_same_element([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% ------------------------------

encode([],[]).

encode(L,X) :-
    pack_same_element(L,PackedList),
    compress(PackedList,X).

compress([],[]).
compress([[H|T]|T1],[[H,N]|T2]) :-
    length([H|T],N),
    compress(T1,T2).

%% encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% ------------------------------

pack_same_element([],[]).
pack_same_element([A],[[A]]).
pack_same_element([H1,H2|T],[[H1]|Tail]) :-
    H1 \= H2,
    pack_same_element([H2|T],Tail).

pack_same_element([H,H|T],[[H|SubList]|List]) :-
    pack_same_element([H|T],[SubList|List]).


encode([],[]).

encode(L,X) :-
    pack_same_element(L,PackedList),
    compress(PackedList,X).

compress([],[]).
compress([[H|T]|T1],Ans) :-
    length([H|T],N),
    ( N < 2 ->  Ans=[[H]|T2]; Ans=[[H,N]|T2]),
    compress(T1,T2).

%% ------------------------------