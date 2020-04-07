
%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_STUDY/2ndSem/Ser502/Assignment/Logic_Programming/Assignment3/').

%% Table Function
:- table expr/3, term/3.

program(t_program(X)) --> block(X),[.].
block(t_block(X,Y)) --> [begin], declaration(X), [;], command(Y), [end].

declaration(t_multiple_declaration(X,Y)) --> single_declaration(X), [;], declaration(Y).
declaration(t_single_declaration(X)) --> single_declaration(X).
single_declaration(t_dec_assign_number(X,Y)) --> [const], identifier(X), [=], num(Y).
single_declaration(t_declare_variable(X)) --> [var], identifier(X).

command(t_multiple_command(X,Y)) --> single_command(X), [;], command(Y).
command(t_single_command(X)) --> single_command(X).
single_command(t_comm_assign_expression(X,Y)) --> identifier(X), [:=], expression(Y).
single_command(t_comm_if_then_else(X,Y,Z)) --> [if], boolean_exp(X), [then], command(Y), [else], command(Z), [endif].
single_command(t_comm_while_do(X,Y)) --> [while], boolean_exp(X), [do], command(Y), [endwhile].
single_command(t_comm_program(X)) --> block(X).

boolean_exp(t_boolean_value_true) --> [true].
boolean_exp(t_boolean_value_false) --> [false].
boolean_exp(t_boolean_exp_equal(X,Y)) --> expression(X), [=], expression(Y).
boolean_exp(t_boolean_exp_not(X)) --> [not], boolean_exp(X).


expression(t_assign_multiple_expression(X,Y)) --> identifier(X), [:=], expression(Y).
expression(X) --> expr(X).

expr(t_add_expr(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub_expr(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

term(t_mul_expr(X,Y)) --> term(X), [*], term_bracket(Y).
term(t_div_expr(X,Y)) --> term(X), [/], term_bracket(Y).
term(X) --> term_bracket(X).

term_bracket(t_bracket_expr(X)) --> ['('], expr(X), [')'].
term_bracket(X) --> identifier(X).
term_bracket(X) --> num(X).

identifier(t_id(x)) --> [x].
identifier(t_id(y)) --> [y].
identifier(t_id(z)) --> [z].
identifier(t_id(u)) --> [u].
identifier(t_id(v)) --> [v].

num(t_num(X)) --> [X], {number(X)}.



%% Semantics Start Here

program_eval(t_program(B), XVal, YVal, ZVal) :-
	update(x, XVal, [], Env1),
	update(y, YVal, Env1, Env2),
	eval_block(B, Env2, Env3),
	lookup(z, Env3, ZVal).

eval_block(t_block(DL,CL), Env, EnvRes) :-
	eval_declaration_list(Dl, Env, Env1),
	eval_command_list(Cl, Env1, EnvRes).

eval_declaration_list(t_multiple_declaration(D,DL), Env, EnvR) :-
	eval_declaration(D, Env, Env1),
	eval_declaration_list(DL, Env1, EnvR), ! .
eval_declaration_list(t_single_declaration(D), Env, EnvR) :-
	eval_declaration(D, Env, EnvR).

eval_declaration(t_dec_assign_number(X,Y), Env, EnvRes) :-
	update(X, Y, Env, EnvRe).

%% eval_command_list(Empty, Env, Env)
eval_command_list(t_multiple_command(C,CL), Env, EnvR) :-
	eval_command(C, Env, Env1),
	eval_command_list(CL, Env1, EnvR), ! .
eval_command_list(t_single_command(C), Env, EnvR) :-
	eval_command(C, Env, EnvR).

eval_command(t_comm_assign_expression(I,E), Env, NewEnv) :-
	eval_expression(E, Env, Env1, Val),
	update(I, Val, Env1, NewEnv).


eval_command(t_comm_while_do(B,C), Env, NewEnv) :-
	eval_boolean_expression(B, Env, true),
	eval_command(C, Env, Env1),
	eval_command(t_comm_while_do(B,C), Env1, NewEnv).
eval_command(t_comm_while_do(B,_C), Env, Env) :- 
	eval_boolean_expression(B, Env, false).

eval_command(t_comm_if_then_else(B,C1,C2), Env, NewEnv) :-
	eval_boolean_expression(B, Env, true),
	eval_command(C1, Env, NewEnv).

eval_command(t_comm_if_then_else(B,C1,C2), Env, NewEnv) :-
	eval_boolean_expression(B, Env, false),
	eval_command(C2, Env, NewEnv).


eval_boolean_expression(t_boolean_exp_equal(E1,E2), Env, Val) :-
	eval_expression(E1, Env, Env1, Val1),
	eval_expression(E2, Env1, Env2, Val2),
	equal(Val1,Val2,Val).

equal(Val1,Val2,true) :- Val1=Val2.
equal(Val1,Val2,false) :- Val1\=Val2.

eval_boolean_expression(t_boolean_exp_not(B), Env, Val) :-
	eval_boolean_expression(B,Env,Val1),
	not(Val1,Val).

not(true,false).
not(false,true).

eval_boolean_expression(t_boolean_value_true, _, true).
eval_boolean_expression(t_boolean_value_false, _, false).

eval_expression(t_assign_multiple_expression(I,E), Env, EnvRes,Val) :-
	eval_expression(E, Env, Env1, Val),
	update(I, Val, Env1, EnvRes).

eval_expression(t_add_expr(X,Y), Env, Val) :- 
	eval_expression(X, Env, Val1),
    eval_expression(Y, Env, Val2),
    Val is Val1 + Val2.

eval_expr(t_sub_expr(X,Y), Env, Val) :- eval_expr(X, Env, Val1),
    								eval_expr(Y, Env, Val2),
    								Val is Val1 - Val2.

eval_expr(t_mul_expr(X,Y), Env, Val) :- eval_expr(X, Env, Val1),
    								eval_expr(Y, Env, Val2),
    								Val is Val1 * Val2.

eval_expr(t_div_expr(X,Y), Env, Val) :- eval_expr(X, Env, Val1),
    								eval_expr(Y, Env, Val2),
    								Val is Val1 / Val2.

eval_expr(t_bracket_expr(X), Env, Val) :- eval_expr(X, Env, Val).


eval_expr(t_id_expr(X), Env, Val) :- lookup(X, Env, Val).

eval_expr(t_num_expr(X),_,X).


%% Problems
%% 	- single_command(t_program(X)) --> block(X).
%% 	- eval_command_list(CL, Env1, EnvR), ! .
%%  - skipping single_command(t_comm_program(X)) --> block(X). semantics 
%%  - eval_boolean_expression(t_boolean_exp_equal(E1,E2), Env, Val) :-



