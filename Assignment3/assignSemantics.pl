

program(t_program(X,.)) --> block(X),[.].
block(t_block(X,Y)) --> [begin], declaration(X), [;], command(Y), [end].

declaration(t_declaration(X,Y)) --> single_declaration(X), [;], declaration(Y).
declaration(t_declaration(X)) --> single_declaration(X).
single_declaration(t_single_declaration(const,X,=,Y)) --> [const], identifier(X), [=], number(Y).
single_declaration(t_single_declaration(var,X)) --> [var], identifier(X).

command(t_command(X,;,Y)) --> single_command(X), [;], command(Y).
command(t_command(X)) --> single_command(X).
single_command(t_single_command(X,:=,Y)) --> identifier(X), [:=], expression(Y).
single_command(t_single_command(if,X,then,Y,else,Z,endif)) --> [if], boolean_exp(X), [then], command(Y), [else], command(Z), [endif].
single_command(t_single_command(while,X,do,Y,endwhile)) --> [while], boolean_exp(X), [do], command(Y), [endwhile].
single_command(t_single_command(X)) --> block(X).

boolean_exp(t_boolean_exp(true)) --> [true].
boolean_exp(t_boolean_exp(false)) --> [false].
boolean_exp(t_boolean_exp(X,=,Y)) --> expression(X), [=], expression(Y).
boolean_exp(t_boolean_exp(not,X)) --> [not], boolean_exp(X).


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