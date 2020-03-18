
%% Printing Disk Moved From -> To 
hanoi(1,From,To,_) :-
	write('Move '),
	write(From),
	write(' '),
	write('to '),
	write(To),
	nl.

%% Predicate to calculate way to transfer disk from source to destination using intermediate peg
hanoi(N,Source,Destination,Intermediate) :-
	N>1,
	N1 is N-1,
	hanoi(N1,Source,Intermediate,Destination),
	hanoi(1,Source,Destination,_), %% Method specifically to call print Method
	hanoi(N1,Intermediate,Destination,Source).