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


next_prime(CurrentPrime,NextPrime) :-
	Val is CurrentPrime + 2,
	is_prime(Val),
	NextPrime = Val.

next_prime(Current,CurrentNext) :-
	N is Current + 2,
	not(is_prime(N)),
	next_prime(N,CurrentNext).

goldbach(Input,Result) :-
	Input>3,
	Input mod 2 =:=0,
	(Input =:= 4 -> Result=[2,2];goldbach(Input,Result,3)).

goldbach(Input,Result,First) :-
	Second is Input - First,
	First=<Second,
	is_prime(Second),
	Result = [First,Second].

goldbach(Input,Result,First) :-
	First<Input,
	next_prime(First,Second),
	goldbach(Input,Result,Second).