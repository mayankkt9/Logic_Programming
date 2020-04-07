
%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_STUDY/2ndSem/Ser502/Assignment/Logic_Programming/Assignment3/').

%% Table Function
:- table expr/3, term/3.

program(t_program(X)) --> block(X),[.].
block(t_block(X,Y)) --> [begin], declaration(X), [;], command(Y), [end].

declaration(t_multiple_declaration(X,Y)) --> single_declaration(X), [;], declaration(Y).
declaration(t_single_declaration(X)) --> single_declaration(X).
single_declaration(t_assign_number(X,Y)) --> [const], identifier(X), [=], num(Y).
single_declaration(t_declare_variable(X)) --> [var], identifier(X).

command(t_multiple_command(X,Y)) --> single_command(X), [;], command(Y).
command(t_single_command(X)) --> single_command(X).
single_command(t_assign_expression(X,Y)) --> identifier(X), [:=], expression(Y).
single_command(t_if_then_else(X,Y,Z)) --> [if], boolean_exp(X), [then], command(Y), [else], command(Z), [endif].
single_command(t_while_do(X,Y)) --> [while], boolean_exp(X), [do], command(Y), [endwhile].
single_command(t_program(X)) --> block(X).

boolean_exp(t_boolean_value(true)) --> [true].
boolean_exp(t_boolean_value(false)) --> [false].
boolean_exp(t_boolean_exp_equal(X,=,Y)) --> expression(X), [=], expression(Y).
boolean_exp(t_boolean_exp_not(not,X)) --> [not], boolean_exp(X).


expression(t_assign_multiple_expression(X,Y)) --> identifier(X), [:=], expression(Y).
expression(X) --> expr(X).

expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

term(t_mul(X,Y)) --> term(X), [*], term_bracket(Y).
term(t_div(X,Y)) --> term(X), [/], term_bracket(Y).
term(X) --> term_bracket(X).

term_bracket(t_bracket(X)) --> ['('], expr(X), [')'].
term_bracket(X) --> identifier(X).
term_bracket(X) --> num(X).

identifier(t_id(x)) --> [x].
identifier(t_id(y)) --> [y].
identifier(t_id(z)) --> [z].
identifier(t_id(u)) --> [u].
identifier(t_id(v)) --> [v].

num(t_num(X)) --> [X], {number(X)}.