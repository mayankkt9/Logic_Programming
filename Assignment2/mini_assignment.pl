%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_Study/2nd_Sem/SER_502/Prolog_Workspace/Assignment2').
%% [mini_assignment].

sentence --> noun_phrase,verb_phrase.
noun_phrase --> article,noun.
article --> [a].
article --> [the].
noun --> [girl].
noun --> [dog].
verb_phrase --> verb,noun_phrase.
verb --> [sees].
verb --> [pets].


sentence_tree(sentence(X,Y)) --> noun_phrase_tree(X),verb_phrase_tree(Y).
noun_phrase_tree(noun_phrase(X,Y)) --> article_tree(X),noun_tree(Y).
article_tree(article(a)) --> [a].
article_tree(article(the)) --> [the].
noun_tree(noun(girl)) --> [girl].
noun_tree(noun(dog)) --> [dog].
verb_phrase_tree(verb_phrase(X,Y)) --> verb_tree(X),noun_phrase_tree(Y).
verb_tree(verb(sees)) --> [sees].
verb_tree(verb(pets)) --> [pets].


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