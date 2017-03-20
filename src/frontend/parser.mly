%{
  open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK DBL_QUOTE SNG_QUOTE
%token SEMI COMMA DOTDOT DOT PLUS MINUS TIMES DIVIDE MOD EXPONENT
%token ASSIGN ADD_ASN SUB_ASN MUL_ASN DIV_ASN MOD_ASN EXP_ASN
%token LOG_AND LOG_OR LOG_NOT LT GT EQ NEQ LEQ GEQ
%token QUES COLON FILTER MAP FUNC IF THEN ELIF ELSE FOR WHILE DO
%token NS GN KN STRUCT LET VAR INT_T FLOAT_T STRING_T BOOL_T VECTOR_T

%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

%nonassoc ELSE
%nonassoc LT GT
%right ASSIGN
%left OR
%left AND
%left EQ NEQ LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start program

%type <Ast.program> program

%%

program:
  /* nothing */ EOF                     { ([], [], []) }
| ns_decls let_decls fn_decls EOF       { ($1, $2, $3) }

ns_decls:
  ns_decls ns_decl						          { $2::$1 }
| ns_decl								                { [$1] }

let_decls:
  let_decls let_decl                    { $2::$1 }  
| let_decl								              { [$1] }

fn_decls: 
  fn_decls fn_decl						          { $2::$1 }
| fn_decl								                { [$1] } 

ns_decl:
  NS ID ASSIGN LBRACE program RBRACE    { {nname = $2; body = $5} }

let_decl:
  LET typ ID ASSIGN expr SEMI			      { (($2, $3), $5) } /* might not work, need to eval expr */
| LET STRUCT ID ASSIGN LBRACE struct_def RBRACE
                                        { {sname = $3; fields = $6} }

struct_def:
  struct_def val_decl SEMI				      { $2::$1 }
| val_decl SEMI							            { [$1] }

fn_decl:
  fn_type ID LPAREN formals RPAREN ret_type LBRACE statements ret_expr RBRACE 
                                        { {fname = $2; fn_typ = $1; ret_typ = $6;
                                          formals = $4; body = $8; ret_expr = $9} } 

statements:								 
  statements statement SEMI				      { $2::$1 }
| statement SEMI						            { [$1] } 

statement:
  decl									                { $1 }
| expr									                { $1 } 

ret_expr:
 expr									                  { $1 } 
| /* nothing */ 						            { Noexpr }

expr:
  asn_expr								              { $1 } 

asn_expr:
  unary_expr ASSIGN asn_expr			      { Binop ($1, Asn, $3) }
| conditional_expr						          { $1 } 

conditional_expr:
  fn_expr COLON conditional_expr        { Cond(fn_expr, fn_expr, conditional_expr) }
| bool_expr QUES fn_expr COLON conditional_expr
                                        { Cond($1, $3, $5) }
| IF bool_expr THEN fn_expr ELSE conditional_expr
                                        { Cond($2, $4, $6) } 

fn_expr:
  iter_expr fn_op kns					{ [] } 
| iter_expr								{ $1 } 

kns:
  kn kns								{ [] } 
| kn									{ [] } 

kn:
  ID									{ Id($1) } 
| LPAREN formals RPAREN FUNC LBRACE statements ret_expr RBRACE { [] } 

iter_expr:
  iter_type unit_expr gn				{ [] }
| unit_expr								{ $1 } 

gn:
 ID LPAREN actuals RPAREN				{ [] }

/* all the stuff below is pretty much taken and adapted from K&R */
unit_expr:
  bool_expr								{ $1 } 

bool_expr:
  bool_or_expr							{ $1 } 

bool_or_expr:
  bool_or_expr LOG_OR bool_and_expr		{ [] } 
| bool_and_expr							{ [] } 

bool_and_expr:
  bool_and_expr LOG_AND cmp_expr		{ [] }
| cmp_expr								{ $1 }

cmp_expr:
  eq_expr								{ $1 } 

eq_expr:
  eq_expr eq_op relat_expr				{ [] }
| relat_expr							{ [] }

relat_expr:
  shift_expr relat_op shift_expr		{ [] }
| shift_expr							{ $1 } 

shift_expr:
  arithmetic_expr						{ $1 } 

arithmetic_expr:
  add_expr								{ $1 } 

add_expr:
  add_expr add_op mult_expr				{ $1 }
| mult_expr								{ $1 }

mult_expr:
  mult_expr mult_op unary_expr			{ $1 }
| unary_expr							{ $1 }

unary_expr:
  unary_op postfix_expr					{ [] }
| postfix_expr							{ $1 } 

postfix_expr:
  postfix_expr postfix					{ [] }
| primary_expr							{ $1 }

postfix:
  LBRACK expr RBRACK					{ [] }
| LPAREN actuals RPAREN					{ [] }
| DOT INT_LIT							{ [] } 
| DOTDOT INT_LIT						{ [] }

primary_expr:
  ID									{ Id($1) }
| lit									{ [] }
| LPAREN expr RPAREN					{ [] } 

formals:
  formal_list							{ [$1] } 
| /* nothing */							{ Return Noexpr }

formal_list:
  val_decl COMMA formal_list			{ [] }
| val_decl								{ $1 } 

actuals:
  actual_list							{ [$1] }
| /* nothing */							{ Return Noexpr } 

actual_list:
  expr COMMA actual_list				{ [] }
| expr									{ [$1] } 

decl:
  decl_mod val_decl						{ [] }
| decl_mod val_decl asn_op expr			{ [] }

asn_op:
  ASSIGN								{ Asn } 
| ADD_ASN 								{ []  }
| SUB_ASN 								{ []  }
| MUL_ASN 								{ []  }
| DIV_ASN 								{ []  }
| MOD_ASN 								{ []  }
| EXP_ASN								{ []  }

decl_mod:
  VAR									{ [] }
| /* nothing, val */					{ Return Noexpr }

val_decl:
  typ ID								{ [] }

ret_type:
	typ									{ [] }
typ:
  primitive_t array_t					{ [] } 
| STRUCT ID array_t						{ [] }
/* user defined structs */ 

primitive_t:
  INT_T									{ [] }
| FLOAT_T								{ [] }
| STRING_T								{ [] }
| BOOL_T								{ [] }
| vector_t								{ [] }

vector_t:
  VECTOR_T LT INT_LIT GT				{ [] }

array_t:
  LBRACK RBRACK array_t					{ [] }
| /* optional */						{ [] }

lit:
  struct_lit							{ [] } 
| array_lit								{ [] }
| vector_lit							{ [] }
| STRING_LIT							{ [] }
| BOOL_LIT								{ [] }
| FLOAT_LIT								{ [] }

struct_lit:	
  LBRACE struct_lit_fields RBRACE		{ [] }
| LBRACE RBRACE							{ [] }

struct_lit_fields:
  struct_lit_field struct_lit_fields	{ [] }
| struct_lit_field						{ [] }

struct_lit_field:
  DOT ID ASSIGN expr					{ [] }

array_lit:
  LBRACK list_lit_elements RBRACK		{ [] }
| LBRACK RBRACK							{ [] }

vector_lit:
  LT list_lit_elements GT				{ [] }
| LT GT									{ [] }

list_lit_elements:
  expr COMMA list_lit_elements			{ [] }
| expr									{ [] }

eq_op:
  EQ									{ [] }
| NEQ									{ [] }

relat_op:
  LT									{ [] }
| GT									{ [] }
| LEQ									{ [] }
| GEQ									{ [] }

add_op:
  PLUS									{ [] }
| MINUS									{ [] }

mult_op:
  TIMES									{ [] }
| DIVIDE								{ [] }
| MOD 									{ [] }

unary_op:
  PLUS									{ [] }
| MINUS									{ [] }
| LOG_NOT								{ [] } 

fn_op:
  MAP									{ [] }
| FILTER								{ [] }

fn_type:
  GN									{ [] }
| KN									{ [] }

iter_type:
  FOR									{ [] }
| DO									{ [] }
