Practice Problem and Understand


%% ------------------------------
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

%% member(6,[1,2,3,4,5]).
%% ------------------------------
