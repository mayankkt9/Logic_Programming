%% Practice Problem and Understand

%% Sample 1 - member
%% Sample 2 - sorted
%% Sample 3 - list_length
%% Sample 4 - copy_list
%% Sample 5 - append list
%% Sample 6 - delete list 
%% Sample 7 - select list 
%% Sample 8 - permutaion
%% Sample 9 - permutaion sort
%% Sample 10 - lookup
%% Sample 11 - update
%% Sample 12 - gcd

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
%% Program 14 - duplicate  
%% Program 15 - N duplicate 
%% Program 17 - split
%% Program 18 - slice (Nice Question)
%% Program 19 - rotate
%% Program 20 - remove_at
%% Program 21 - insert_at
%% Program 22 - range numbers
%% Program 23 - rnd_select
%% Program 24 - rnd_select_2
%% Program 25 - rnd_permu

%% TODO
%% Prefix | suffix
%% insertion
%% quick


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


append_list([],L,L).
append_list([H|T],L1,[H|Ans]) :-
    append_list(T,L1,Ans).


%% append_list([1,2,3],[a,b],L).
%% ------------------------------


delete_list(_,[],[]).
delete_list(X,[X|T],T1) :- delete_list(X,T,T1).
delete_list(X,[H|T],[H|Ans]) :-delete_list(X,T,Ans).

%% delete_list(2,[1,2,3,1,2,1,5],L).
%% ------------------------------


permutation([],[]).
permutation(L,[H|T]) :-
    select(H,L,R),
    permutation(R,T).

%% permutation([1,2,3],L).
%% ------------------------------

permutation_sort(X,Y) :-
    permutation(X,Y),
    sorted(Y).
%% ------------------------------


lookup(Key,[(Key,Value)|_],Value).
lookup(Key,[(K1,_)|T],V):-
    Key \= K1,
    lookup(Key,T,V).

%% lookup(5,[(1,10),(2,20),(3,30),(4,40)],X).
%% ------------------------------


update(Key,Value,[],[(Key,Value)]).
update(Key,Value,[(Key,_)|T],[(Key,Value)|T]).
update(Key,Value,[(K1,V1)|T],[(K1,V1)|T2]) :-
    Key \= K1,
    update(Key,Value,T,T2).

%% update(3,35,[(1,10),(2,20),(3,30),(4,40)],X).
%% ------------------------------

gcd(A,0,A).
gcd(A,B,C) :-
    K is A mod B,
    gcd(B,K,C).
%% gcd(30,45,K).
%% ------------------------------


%% ------------------------------------------------------------------------------------------------------------------------------------------------------
%% ------------------------------------------------------------------------------------------------------------------------------------------------------



%% 99 Prolog program




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

%% Non Tail

reverse([],[]).
reverse([H|T],Z) :-
    reverse(T,Z1),
    append(Z1,[H],Z).

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

dupli([],[]).
dupli([H|T],[H,H|T1]) :-
    dupli(T,T1).

%% dupli([a,b,c,c,d],X).
%% ------------------------------



dupli([],_,[]).
dupli([H|T],X,Ans) :-
    get(H,X,L),
    dupli(T,X,Ans1),
    append(L,Ans1,Ans).
    

get(_,0,[]).
get(X,N,[X|T]) :-
    N > 0,
    N1 is N - 1,
    get(X,N1,T).

%% dupli([a,b,c],3,X).
%% ------------------------------


split(L,0,[],L).
split([H|T],N,[H|T1],L2) :-
    N>0,
    N1 is N - 1,
    split(T,N1,T1,L2).

split([a,b,c,d,e,f,g,h,i,k],5,L1,L2).


%% split([a,b,c,d,e,f,g,h,i,k],5,L1,L2).
%% ------------------------------
slice(L,S,E,A) :-
    slice_me(L,0,S,E,A).

slice_me([H|_],_,1,1,[H]).
slice_me([H|T],C,1,E,[H|A]) :-
    E>1,
    E1 is E - 1,
    slice_me(T,C,1,E1,A).

slice_me([_|T],C,S,E,A) :-
    S>1,
    S1 is S - 1,
    E1 is E - 1,
    slice_me(T,C,S1,E1,A).

%% slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%% ------------------------------


split(L,0,[],L).
split([H|T],N,[H|T1],L2) :-
    N>0,
    N1 is N - 1,
    split(T,N1,T1,L2).


rotate(L,0,L).
rotate(L,N,X) :-
    N>0,
    rotate_me(L,N,X).
rotate(L,N,X) :-
    N=<0,
    length(L,N1),
    N2 is N1 + N,
    rotate_me(L,N2,X).

rotate_me(L,N,X) :-
    split(L,N,L1,L2),
    append(L2,L1,X).

%% rotate([a,b,c,d,e,f,g,h],-3,X).
%% ------------------------------

remove_at(H,[H|T],1,T).
remove_at(X,[H|T],N,[H|Ans]) :-
    N1 is N-1,
    N>0,
    remove_at(X,T,N1,Ans).

%% remove_at(X,[a,b,c,d],2,R).
%% ------------------------------

insert_at(X,L,1,[X|L]).
insert_at(X,[H|T],N,[H|Ans]) :-
    N1 is N-1,
    N>0,
    insert_at(X,T,N1,Ans).

%% insert_at(alfa,[a,b,c,d],2,L).
%% ------------------------------

range(E,E,[E]).
range(S,E,[S|T]) :-
    S1 is S + 1,
    range(S1,E,T).

%% range(4,9,L).
%% ------------------------------

remove_at(H,[H|T],1,T).
remove_at(X,[H|T],N,[H|Ans]) :-
    N1 is N-1,
    N>0,
    remove_at(X,T,N1,Ans).


rnd_select(_,0,[]).
rnd_select(L,N,[A|Ans]) :-
    length(L,Len),
    R1 is random(Len)+1,
    remove_at(A,L,R1,L1),
    N1 is N-1,
    rnd_select(L1,N1,Ans).

%% rnd_select([a,b,c,d,e,f,g,h],3,L).
%% ------------------------------

range(E,E,[E]).
range(S,E,[S|T]) :-
    S1 is S + 1,
    range(S1,E,T).

remove_at(H,[H|T],1,T).
remove_at(X,[H|T],N,[H|Ans]) :-
    N1 is N-1,
    N>0,
    remove_at(X,T,N1,Ans).


rnd_select2(0,_,[]).
rnd_select2(Total,N,Ans) :-
    range(1,N,L),
    rnd_select(L,Total,Ans).


rnd_select(_,0,[]).
rnd_select(L,N,[A|Ans]) :-
    length(L,Len),
    R1 is random(Len)+1,
    remove_at(A,L,R1,L1),
    N1 is N-1,
    rnd_select(L1,N1,Ans).

%% rnd_select2(6,49,L).
%% ------------------------------

remove_at(H,[H|T],1,T).
remove_at(X,[H|T],N,[H|Ans]) :-
    N1 is N-1,
    N>0,
    remove_at(X,T,N1,Ans).

rnd_select(_,0,[]).
rnd_select(L,N,[A|Ans]) :-
    length(L,Len),
    Len>0,
    R1 is random(Len)+1,
    remove_at(A,L,R1,L1),
    N1 is N-1,
    rnd_select(L1,N1,Ans).


rnd_permu(L,A) :-
    length(L,N),
    rnd_select(L,N,A).

%% rnd_permu([a,b,c,d,e,f],L).
%% ------------------------------
