%% Palindrome DCG
%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_Study/2nd_Sem/SER_502/Prolog_Workspace/Assignment2').



px --> [].
px --> [0].
px --> [1].
px --> [0],px,[0].
px --> [1],px,[1].

p(t(e)) --> [].
p(t(0)) --> [0].
p(t(1)) --> [1].
p(t(0,X,0)) --> [0],p(X),[0].
p(t(1,X,1)) --> [1],p(X),[1].



%% sentence --> noun_ph,verb_ph.
%% noun_ph --> article,noun.
%% article --> [a].
%% article --> [the].
%% noun --> [girl].
%% noun --> [dog].
%% verb_ph --> verb,noun_ph.
%% verb --> [sees].
%% verb --> [pets].


sentence(sentence(X,Y)) --> noun-phrase(X),verb-phrase(Y).
noun-phrase(noun-phrase(X,Y)) --> article(X),noun(Y).
verb-phrase(verb-phrase(X,Y)) --> verb(X),noun-phrase(Y).
article(article(a)) --> [a].
article(article(the)) --> [the].
noun(noun(girl)) --> [girl].
noun(noun(dog)) --> [dog].
verb(verb(sees)) --> [sees].
verb(verb(pets)) --> [pets].



expr --> number,expr_n.
expr --> ['('],expr,[')'],expr_n.
expr_n --> [].
expr_n --> [+],expr,expr_n.
expr_n --> [*],expr,expr_n.
number --> digit,number_n.
number_n --> [].
number_n --> digit,number_n.
digit --> [1].
digit --> [2].
digit --> [3].
digit --> [4].
digit --> [5].
digit --> [6].
digit --> [7].
digit --> [8].
digit --> [9].
digit --> [0].

