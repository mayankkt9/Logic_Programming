
%% working_directory(CWD,'/Users/mayankkataruka/Desktop/Work/ASU_STUDY/2ndSem/Ser502/Assignment/Logic_Programming/Assignment3/').
%% [assignSemantics].

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

term_bracket(t_bracket_expr(X)) --> ['('], expression(X), [')'].
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
	eval_declaration_list(DL, Env, Env1),
	eval_command_list(CL, Env1, EnvRes).

eval_declaration_list(t_multiple_declaration(D,DL), Env, EnvR) :-
	eval_declaration(D, Env, Env1),
	eval_declaration_list(DL, Env1, EnvR), ! .
eval_declaration_list(t_single_declaration(D), Env, EnvR) :-
	eval_declaration(D, Env, EnvR).

eval_declaration(t_dec_assign_number(X,Y), Env, EnvRes) :-
	update(X, Y, Env, EnvRes).

eval_declaration(t_declare_variable(_), Env, Env).

%% eval_command_list(Empty, Env, Env)
eval_command_list(t_multiple_command(C,CL), Env, EnvR) :-
	eval_command(C, Env, Env1),
	eval_command_list(CL, Env1, EnvR), ! .
eval_command_list(t_single_command(C), Env, EnvR) :-
	eval_command(C, Env, EnvR).

eval_command(t_comm_assign_expression(I,E), Env, NewEnv) :-
	eval_identifier_name(I, Env, Variable),
	eval_expression(E, Env, Env1, Val),
	update(Variable, Val, Env1, NewEnv).

eval_command(t_comm_while_do(B,C), Env, NewEnv) :-
	eval_boolean_expression(B, Env, Env1, true),
	eval_command_list(C, Env1, Env2),
	eval_command(t_comm_while_do(B,C), Env2, NewEnv).
eval_command(t_comm_while_do(B,_C), Env, EnvRes) :- 
	eval_boolean_expression(B, Env, EnvRes, false).

eval_command(t_comm_if_then_else(B,C1,_), Env, NewEnv) :-
	eval_boolean_expression(B, Env, Env1, true),
	eval_command_list(C1, Env1, NewEnv).

eval_command(t_comm_if_then_else(B,_,C2), Env, NewEnv) :-
	eval_boolean_expression(B, Env, Env1, false),
	eval_command_list(C2, Env1, NewEnv).

eval_identifier_name(t_id(X), _, X).

eval_boolean_expression(t_boolean_exp_equal(E1,E2), Env, EnvRes, Val) :-
	eval_expression(E1, Env, Env1, Val1),
	eval_expression(E2, Env1, EnvRes, Val2),
	equal_expression(Val1,Val2,Val).

eval_boolean_expression(t_boolean_exp_not(B), Env, EnvRes, Val) :-
	eval_boolean_expression(B,Env,EnvRes,Val1),
	not_expression(Val1,Val).

eval_boolean_expression(t_boolean_value_true, _, true).
eval_boolean_expression(t_boolean_value_false, _, false).

equal_expression(Val1,Val2,true) :- Val1=Val2.
equal_expression(Val1,Val2,false) :- Val1\=Val2.

not_expression(true,false).
not_expression(false,true).



eval_expression(t_assign_multiple_expression(I,E), Env, EnvRes,Val) :-
	eval_identifier_name(I, Env, Variable),
	eval_expression(E, Env, Env1, Val),
	update(Variable, Val, Env1, EnvRes).

eval_expression(t_add_expr(X,Y), Env, Env2, Val) :- 
	eval_expression(X, Env, Env1, Val1),
    eval_expression(Y, Env1, Env2, Val2),
    Val is Val1 + Val2.

eval_expression(t_sub_expr(X,Y), Env, Env2, Val) :- 
	eval_expression(X, Env, Env1, Val1),
    eval_expression(Y, Env1, Env2, Val2),
    Val is Val1 - Val2.

eval_expression(t_mul_expr(X,Y), Env, Env2, Val) :- 
	eval_expression(X, Env, Env1, Val1),
    eval_expression(Y, Env1, Env2, Val2),
    Val is Val1 * Val2.

eval_expression(t_div_expr(X,Y), Env, Env2, Val) :- 
	eval_expression(X, Env, Env1, Val1),
    eval_expression(Y, Env1, Env2, Val2),
    Val is Val1 / Val2.

eval_expression(t_bracket_expr(X), Env, EnvRes, Val) :- 
	eval_expression(X, Env, EnvRes, Val).


eval_expression(t_id(X), Env, Env, Val) :- 
	lookup(X, Env, Val).

eval_expression(t_num(X), Env, Env, X).


%% Lookup Function 

lookup(Key,[(Key,Value)|_],Value).
lookup(Key,[_|Tail],Value) :- lookup(Key, Tail, Value).

%% Update Function

update(Key, Val, [], [(Key,Val)]).
update(Key, Val, [(Key,_)|Tail], [(Key, Val)|Tail]).
update(Key, Val, [Head|Tail], [Head|Result]) :-
	Head \= (Key,_),
	update(Key, Val, Tail, Result). 





%% Problems
%% 	- single_command(t_program(X)) --> block(X).
%% 	- eval_command_list(CL, Env1, EnvR), ! .
%%  - skipping single_command(t_comm_program(X)) --> block(X). semantics 
%%  - eval_boolean_expression(t_boolean_exp_equal(E1,E2), Env, Val) :-
%%  - test case 3 Failing

/**
Test Case 1

begin
	var z;
	var x;
	z:=x
end.

?- program(P, [begin, var, z, ; , var, x, ;, z, :=, x, end, .], []),program_eval(P, 2, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(z)), t_single_declaration(t_declare_variable(t_id(x)))), t_single_command(t_comm_assign_expression(t_id(z), t_id(x))))),
Z = 2 ;

Test Case 2

begin 
	var x; 
	var y; 
	var z; 
	z:=x+y 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, z,:=,x,+,y, end,.], []),program_eval(P, 2, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_single_command(t_comm_assign_expression(t_id(z), t_add_expr(t_id(x), t_id(y)))))),
Z = 5 ;

Test Case 3

begin 
	var x; 
	var y; 
	var z; 
	z:=(z:=x+2)+y 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, z,:=,'(',z,:=,x,+,2,')',+,y, end,.], []),program_eval(P, 2, 3, Z).
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_single_command(t_comm_assign_expression(t_id(z), t_add_expr(t_bracket_expr(t_assign_multiple_expression(t_id(z), t_add_expr(t_id(x), t_num(2)))), t_id(y)))))),
Z = 7 ;




Test Case 4

begin 
	var x; 
	var y; 
	var z; 
	if x=y then 
		z:=1 
	else 
		z:=0 
	endif 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, if, x,=,y, then, z,:=,1, else, z,:=,0, endif, end,.], []),program_eval(P, 2, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_single_command(t_comm_if_then_else(t_boolean_exp_equal(t_id(x), t_id(y)), t_single_command(t_comm_assign_expression(t_id(z), t_num(1))), t_single_command(t_comm_assign_expression(t_id(z), t_num(0))))))),
Z = 0 ;


Test Case 5

begin 
	var x; 
	var y; 
	var z; 
	if x = 0 then 
		z:=x 
	else 
		z:=y 
	endif 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, if, x, =, 0, then, z,:=,x, else, z,:=,y, endif, end,.], []),program_eval(P, 2, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_single_command(t_comm_if_then_else(t_boolean_exp_equal(t_id(x), t_num(0)), t_single_command(t_comm_assign_expression(t_id(z), t_id(x))), t_single_command(t_comm_assign_expression(t_id(z), t_id(y))))))),
Z = 3 ;


Test Case 6

begin 
	var x; 
	var y; 
	var z; 
	if not x=y then 
		z:=x 
	else 
		z:=y 
	endif 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, if, not, x,=,y, then, z,:=,x, else, z,:=,y, endif, end,.], []),program_eval(P, 2, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_single_command(t_comm_if_then_else(t_boolean_exp_not(t_boolean_exp_equal(t_id(x), t_id(y))), t_single_command(t_comm_assign_expression(t_id(z), t_id(x))), t_single_command(t_comm_assign_expression(t_id(z), t_id(y))))))),
Z = 2 ;



Test Case 7 

begin 
	var x; 
	var z; 
	z:=0; 
	while not x=0 do 
		z := z+1; 
		x:=x-1 
	endwhile 
end.

?- program(P, [begin, var, x,;, var, z,;, z,:=,0,;, while, not, x,=,0, do, z, :=, z,+,1,;, x,:=,x,-,1, endwhile, end,.], []),program_eval(P, 5, 3, Z). 
program(P, [begin, var, x,;, var, z,;, z,:=,0,;, while, not, x,=,0, do, z, :=, z,+,1,;, x,:=,x,-,1, endwhile, end,.], []),program_eval(P, 5, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_single_declaration(t_declare_variable(t_id(z)))), t_multiple_command(t_comm_assign_expression(t_id(z), t_num(0)), t_single_command(t_comm_while_do(t_boolean_exp_not(t_boolean_exp_equal(t_id(x), t_num(0))), t_multiple_command(t_comm_assign_expression(t_id(z), t_add_expr(t_id(z), t_num(1))), t_single_command(t_comm_assign_expression(t_id(x), t_sub_expr(t_id(x), t_num(1)))))))))),
Z = 5 ;


Test Case 8

begin 
	var x; 
	var y; 
	var z; 
	z:=1; 
	u:=x; 
	while not u = 0 do 
		z :=z*y; 
		u:=u-1 
	endwhile 
end.

?- program(P, [begin, var, x,;, var, y,;, var, z,;, z,:=,1,;, u,:=,x,;, while, not, u, =, 0, do, z, :=,z,*,y,;, u,:=,u,-,1, endwhile, end,.], []),program_eval(P, 5, 3, Z). 
P = t_program(t_block(t_multiple_declaration(t_declare_variable(t_id(x)), t_multiple_declaration(t_declare_variable(t_id(y)), t_single_declaration(t_declare_variable(t_id(z))))), t_multiple_command(t_comm_assign_expression(t_id(z), t_num(1)), t_multiple_command(t_comm_assign_expression(t_id(u), t_id(x)), t_single_command(t_comm_while_do(t_boolean_exp_not(t_boolean_exp_equal(t_id(u), t_num(0))), t_multiple_command(t_comm_assign_expression(t_id(z), t_mul_expr(t_id(z), t_id(y))), t_single_command(t_comm_assign_expression(t_id(u), t_sub_expr(t_id(u), t_num(1))))))))))),
Z = 243 .



Test Cases Failing List 





**/
