%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_Study/2nd_Sem/SER_502/Prolog_Workspace').


%% Sample 1
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Sample 2
sorted([]).
sorted([_]).
sorted([A,B|T]) :- A=<B, sorted([B|T]).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Sample 3
listlen([],0).
listlen([_|T],N) :- 
	listlen(T,N1), 
	N is N1+1.
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Sample 4
copylist([],[]).
copylist([H|T],[H|Result]) :-
	copylist(T,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 1
lastelement(X,[X]).
lastelement(X,[_|T]) :- lastelement(X,T).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 2
secondlast(X,[X|[_]]).
secondlast(X,[_|T]) :- secondlast(X,T).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 3
element_at(X,[X|_],1).
element_at(X,[_|T],K) :- NK is K-1,element_at(X,T,NK).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 4
numofelement(0,[]).
numofelement(X,[_|T]) :- numofelement(Y,T),X is Y+1.
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 5
reverse(X,X,[]).
reverse(X,ACC,[H|T]) :- reverse(X,[H|ACC],T).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 6
palindrom(L) :- 
	reverseList(L,RevL,[]),
	check(L,RevL).
reverseList([],L,L).
reverseList([H|T],RevL,ACC) :- 
	reverseList(T,RevL,[H|ACC]).
check([],[]).
check([H|T1],[H|T2]) :- 
	check(T1,T2).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 7
flat1([],[]).
flat1([H|T],List) :-
	flat1(H,R1),
	flat1(T,R2),
	append(R1,R2,List).
flat1([H|T],[H|List]) :-
	not(is_list(H)),
	flat1(T,List).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 8
compress([],[]).
compress([H],[H]).
compress([H,H|T],Result) :- 
	compress([H|T],Result).
compress([H,Y|T],[H|Result]) :-
	H \== Y,
	compress([Y|T],Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 9
pack([], []).
pack([X], [[X]]).

pack([H1, H2|T], [[H1]|TResult]) :-
	H1 \== H2,
	pack([H2|T], TResult).

pack([H, H|T], [[H|TSubResult]|TResult]) :-
	pack([H|T],[TSubResult|TResult]).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 10
encode(List,Result) :-
	pack(List,PList),
	compressL(PList,Result).

compressL([],[]).
compressL([[H|Tsublist]|TList],[[Num|H]|Tail]) :-
	length([H|Tsublist],Num),
	compressL(TList,Tail).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 16
dropAllNth(List,N,X) :-
	N>0,
	dropAllNth(List,N,1,X).

dropAllNth([],_,_,[]) .
dropAllNth([H|T],N,Counter,Result) :-
	(Counter mod N =:= 0 -> Result=Tail ; Result=[H|Tail]) ,
	Counter1 is Counter + 1,
	dropAllNth(T,N,Counter1,Tail).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 17
splitList(L,0,[],L).
splitList([H|T],N,[H|Tail],L2) :-
	N>0,
	N1 is N-1,
	splitList(T,N1,Tail,L2).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 19
rotate(List,N,Result) :-
   length(List,ListLen), 
   N1 is N mod ListLen, 
   rotate_left(List,N1,Result).

rotate_left(L,0,L).
rotate_left(List,N,Result) :- 
	N > 0, 
	splitList(List,N,L1,L2), 
	append(L2,L1,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 20
%% ?- remove_at(X,[a,b,c,d],2,R).
remove_at(X,List,N,Result) :-
	N1 is N-1,
	splitList(List,N1,L1,L2),
	[X|R] = L2,
	append(L1,R,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 21
insertAtList(Element,List,Index,Result) :-
	I is Index-1,
	splitList(List,I,L1,L2),
	append(L1,[Element],L3),
	append(L3,L2,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 22
%% ?- range(4,9,L).
range(End,End,[End]).
range(Start,End,L) :-
	Start<End,
	L=[Start|Tail],
	S1 is Start+1,
	range(S1,End,Tail).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 23
%% ?- rnd_select([a,b,c,d,e,f,g,h],3,L).

rnd_select(_,0,[]).
rnd_select(List,N,Result) :-
	length(List,Len),
	N=<Len,
	R1 is random(Len)+1,
	remove_at(Num,List,R1,Tail),
	Result = [Num|RS1],
	N1 is N-1,
	rnd_select(Tail,N1,RS1).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 24
%% ?- rangerandom(6,49,L).
rangerandom(N,M,Result) :-
	range(1,M,List),
	rnd_select(List,N,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 25
%% ?- rnd_permu([a,b,c,d,e,f],L).
rnd_permu(List,Result) :-
	length(List,Len),
	rnd_select(List,Len,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 26
%% ?- combination(3,[a,b,c,d,e,f],L).
combination(0,_,[]).
combination(K,List,[H|Tail]) :-
	K > 0,
	helpercomb(H,List,R), 
	K1 is K-1, 
	combination(K1,R,Tail).

helpercomb(H,[H|Tail],Tail).
helpercomb(H,[_|Tail],Result) :- 
	helpercomb(H,Tail,Result).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 31
%% ?- is_prime(7).
is_prime(2).
is_prime(3).
is_prime(N) :-
	integer(N),
	N>3,
	N mod 2 =\= 0,
	not(factor(N,3)).
factor(N,Counter) :- 
	N mod Counter =:= 0.
factor(N,Counter) :- 
	Counter*Counter<N,
	C1 is Counter+2,
	factor(N,C1).
%% -------------------------------------------------------------------------------------------------------------------------------------------

%% Program 32
gcd(0,B,B).
gcd(A,B,Result) :-
	T is B mod A,
	gcd(T,A,Result).
