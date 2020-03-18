
%% Database to convert each digit to words
to_words(0,zero).
to_words(1,one).
to_words(2,two).
to_words(3,three).
to_words(4,four).
to_words(5,five).
to_words(6,six).
to_words(7,seven).
to_words(8,eight).
to_words(9,nine).

%% Predicate handling only zero case
full_words(N) :-
	(N=:=0->write(zero);convert_to_words(N,0)).

%% If number is zero stopping condition
convert_to_words(0,_).

%% Predicate to extract each digit and print it in words
convert_to_words(N,Len) :-
	N > 0,
	N1 is N mod 10,
	N2 is div(N,10),
	L1 is Len + 1,
	convert_to_words(N2,L1),
	to_words(N1,Words),
	write(Words),
	(L1 > 1 -> write('-')).