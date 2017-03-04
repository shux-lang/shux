%{
  open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK DBL_QUOTE SNG_QUOTE
%token SEMI COMMA DOTDOT DOT PLUS MINUS TIMES DIVIDE MOD EXPONENT
%token ASSIGN ADD_ASN SUB_ASN MUL_ASN DIV_ASN MOD_ASN EXP_ASN
%token LOG_AND LOG_OR LOG_NOT LTGT EQ NEQ LEQ GEQ
%token QUES COLON FILTER MAP FUNC IF THEN ELIF ELSE FOR WHILE DO
%token NS GN KN STRUCT LET VAR INT_T FLOAT_T STRING_T BOOL_T VECTOR_T

%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

%start program

%type <Ast.program> program

%%

program:
| (* nothing *) EOF                     { }
| ns_decls let_decls fn_decls EOF       { }

ns_decls:
| ns_decls ns_decl
| ns_decl

let_decls:
| let_decls let_decl
| let_decl

fn_decls: 
| fn_decls fn_decl
| fn_decl

ns_decl:
| NS ID ASSIGN LBRACE program RBRACE    { }

(* need to consider structs *)
let_decl:
| LET type ID ASSIGN expr SEMICOLON { }

fn_decl:
| fn_type ID LPAREN formals RPAREN ret_type RBRACE block LBRACE

block:
| statements ret_expr

statements:
| statement SEMICOLON statements
| statement SEMICOLON
(* do we need these? 
| conditional_statement
| interation_statement
* - j-hui
*)

statement:
| decl
| expr

ret_expr:
| expr
| (* good old bucket of nothing *)
(* warn: this may produce S/R conflicts *)

expr:
| asn_expr

asn_expr:
| unary_expr asn_op asn_expr
| conditional_expr

conditional_expr:
| fn_expr COLON conditional_expr
| bool_expr QUES fn_expr COLON conditional_expr
| IF bool_expr THEN fn_expr ELSE conditional_expr

fn_expr:
| iter_expr fn_op kns
| iter_expr
kns:
| kn kns
| kn
kn:
| ID
| LPAREN formals RPAREN FUNC LBRACE block RBRACE
(*| formals FUNC LBRACE block RBRACE *)
(* do we want lambdas to look like
 * param1, param2 -> { // etc. }
 * or
 * (param1, param2) -> {// etc. }?
 * since this isn't python, the latter looks better imo
 * - J-Hui
 *)

iter_expr:
| iter_type unit_expr gn
| unit_expr
gn:
| ID LPAREN actuals RPAREN
| LPAREN RPAREN (* nop gn *)

(* all the stuff below is pretty much taken and adapted from K&R *)
unit_expr:
| bool_expr

bool_expr:
| bool_or_expr
bool_or_expr:
| bool_or_expr LOG_OR bool_and_expr
| bool_and_expr
bool_and_expr:
| bool_and_expr LOG_AND bit_expr
| bit_expr

(* not implemented *)
bit_expr:
| cmp_expr

cmp_expr:
| eq_expr
eq_expr:
| eq_expr eq_op relat_expr
| relat_expr
(* no recursion because we don't want x < y < z *)
relat_expr:
| shift_expr relat_op shift_expr
| shift_expr

shift_expr:
| arithmetic_expr

arithmetic_expr:
| add_expr
add_expr:
| add_expr add_op mult_expr
| mult_expr
mult_expr:
| mutl_expr mult_op unary_expr
| unary_expr

unary_expr:
| unary_op postfix_expr
| postfix_expr

postfix_expr:
| postfix_expr postfix
| primary_expr
postfix:
| LBRACK expr RBRACK
| LPAREN actuals RPAREN
| DOT INT_LIT
| DOTDOT INT_LIT
| LT actuals GT

primary_expr:
| ID
| lit
| LPAREN expr RPAREN

formals:
| formal_list
|
formal_list:
| val_decl COMMA formal_list
| val_decl

actuals:
| actual_list
|
actual_list:
| expr COMMA actual
| expr

decl:
| decl_mod val_decl

decl_mod:
| VAR
| (* nothing, val *)

val_decl:
| typ ID

typ:
| primitive_t array_t
| ID array_t (* for user-defined structs *)

primitive_t:
| INT_T
| FLOAT_T
| STRING_T
| BOOL_T
| vector_t
vector_t:
| VEC LT INT_LIT GT
array_t:
| LBRACK RBRACK array_t
| (* optional *)

lit:
| struct_lit
| array_lit
| vector_lit
| STRING_LIT
| BOOL_LIT
| FLOAT_LIT

struct_lit:
| LBRACE struct_lit_fields RBRACE
| LBRACE RBRACE
struct_lit_fields:
| struct_lit_field struct_lit_fields
| struct_list_field
struct_lit_field:
| DOT ID ASSIGN expr

array_lit:
| LBRACK list_lit_elements RBRACK
| LBRACK RBRACK

vector_lit:
| LT list_lit_elements GT
| LT GT

list_lit_elements
| expr COMMA list_lit_elements
| expr

eq_op:
| EQ
| NEQ

relat_op:
| LT
| GT
| LEQ
| GEQ

add_op:
| PLUS
| MINUS

mult_op:
| TIMES
| DIVIDE

unary_op:
| PLUS
| MINUS
| LOG_NOT
(* not implemented
| TILDE
*)

fn_op:
| MAP
| FILTER

fn_type:
| GN
| FN

iter_type:
| FOR
| DO

