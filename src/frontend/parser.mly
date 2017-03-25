%{
  open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK DBL_QUOTE SNG_QUOTE
%token SEMI COMMA DOTDOT DOT PLUS MINUS TIMES DIVIDE MOD EXPONENT
%token ASSIGN ADD_ASN SUB_ASN MUL_ASN DIV_ASN MOD_ASN EXP_ASN
%token LOG_AND LOG_OR LOG_NOT LT GT EQ NEQ LEQ GEQ
%token QUES COLON FILTER MAP FUNC IF THEN ELIF ELSE FOR WHILE DO UNDERSCORE
%token NS GN KN STRUCT LET EXTERN VAR INT_T FLOAT_T STRING_T BOOL_T VECTOR_T

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
  | /* nothing */ EOF                       { ([], [], []) }
  | ns_decls let_decls fn_decls EOF         { ($1, $2, $3) }

/* namespace declaration rules */
ns_decls:
  | ns_decls ns_decl                        { $2::$1 }
  | ns_decl                                 { [$1] }

ns_decl:
  | NS ID ASSIGN LBRACE program RBRACE      { {nname = $2; body = $5} }


/* let declaration rules */
let_decls:
  | let_decls let_decl                      { $2::$1 }  
  | let_decl                                { [$1] }

let_decl:
  | LET typ ID ASSIGN expr SEMI             { LetDecl( (Bind (Immutable, $2, $3), $5)) }
  | STRUCT ID LBRACE struct_def RBRACE      { StructDef({sname = $2; fields = $4}) }

struct_def:
  | struct_def val_decl SEMI                { $2::$1 }
  | val_decl SEMI                           { [$1] }


/* function declaration rules */
fn_decls: 
  | fn_decls fn_decl                        { $2::$1 }
  | fn_decl                                 { [$1] } 

fn_decl:
  | fn_type ID LPAREN formals RPAREN ret_typ 
    LBRACE statements ret_expr RBRACE       { {fname = $2; fn_typ = $1; ret_typ = $6;
                                              formals = $4; body = $8; ret_expr = $9} } 
ret_typ:
  | typ                                     { Some($1) }
  | /* void return type */                  { None }

ret_expr:
  | expr                                    { Some($1) } 
  | /* nothing */                           { None }


/* function body rules */
statements:                 
  | statements statement SEMI               { $2::$1 }
  | statement SEMI                          { [$1] } 

statement:
  | decl                                    { $1 }
  | expr                                    { Expr($1) } 

decl:
  | decl_mod typ ID                         { VDecl(Bind($1, $2, $3), None) }
  | decl_mod typ ID ASSIGN expr             { VDecl(Bind($1, $2, $3), Some($5)) }

expr:
  | asn_expr                                { $1 } 

asn_expr:
  | unary_expr asn_op asn_expr              { Binop ($1, $2, $3) }
  | if_expr                                 { $1 } 

if_expr:
  | fn_expr COLON if_expr                   { Cond($1, $1, $3) }
  | bool_expr QUES fn_expr COLON if_expr    { Cond($1, $3, $5) }
  | IF bool_expr THEN fn_expr ELSE if_expr  { Cond($2, $4, $6) } 
  | fn_expr                                 { $1 }

fn_expr:
  | iter_expr fn_op kns                     { Binop($1, $2, $3) } 
  | iter_expr                               { $1 } 

kns:
  | kn fn_op kns                            { Binop($1, $2, $3) }
  | kn                                      { $1 }

kn:
  | id                                      { $1 }
  | lambda                                  { Lit($1) }

lambda:
  | LPAREN formals RPAREN FUNC 
    LBRACE statements ret_expr RBRACE       { LitKn({formals = $2; body = $6; ret_expr = $7}) } 

iter_expr:
  | iter_type unit_expr gn_call             { Binop($2, $1, $3) }
  | unit_expr                               { $1 }

gn_call:
  | ID LPAREN actuals RPAREN                { Call(Some($1), $3) }
  | UNDERSCORE                              { Call(None, []) }

unit_expr:
  | bool_expr                               { $1 } 

bool_expr:
  | bool_or_expr                            { $1 } 

bool_or_expr:
  | bool_or_expr LOG_OR bool_and_expr       { Binop($1, LogOr, $3) } 
  | bool_and_expr                           { $1 } 

bool_and_expr:
  | bool_and_expr LOG_AND cmp_expr          { Binop($1, LogAnd, $3) }
  | cmp_expr                                { $1 }

cmp_expr:
  | eq_expr                                 { $1 }

eq_expr:
  | eq_expr eq_op relat_expr                { Binop($1, $2, $3) }
  | relat_expr                              { $1 }

relat_expr:
  | shift_expr relat_op shift_expr          { Binop($1, $2, $3) }
  | shift_expr                              { $1 }

/* unused, bit-wise operations not implemented */
shift_expr:
  | arithmetic_expr                         { $1 } 

arithmetic_expr:
  | add_expr                                { $1 } 

add_expr:
  | add_expr add_op mult_expr               { Binop($1, $2, $3) }
  | mult_expr                               { $1 }

mult_expr:
  | mult_expr mult_op unary_expr            { Binop($1, $2, $3) }
  | unary_expr                              { $1 }

unary_expr:
  | prefix_op unary_expr                    { Uniop($1, $2) }
  | postfix_expr                            { $1 } 

postfix_expr:
  | postfix_expr LBRACK expr RBRACK         { Binop($1, Index, $3) }
  | ID LPAREN actuals RPAREN                { Call(Some($1), $3) }
  | postfix_expr DOT id                     { Binop($1, StructMember, $3) }
  | primary_expr                            { $1 }

primary_expr:
  | LPAREN expr RPAREN                      { $2 }
  | lit                                     { Lit($1) }
  | id DOTDOT int_lit                       { Binop($1, Lookback, Lit($3)) }
  | id                                      { $1 }


/* function argument rules */
formals:
  | formal_list                             { $1 }
  | /* nothing */                           { [] }

formal_list:
  | val_decl COMMA formal_list              { $1::$3 }
  | val_decl                                { [$1] } 

actuals:
  | actual_list                             { $1 }
  | /* nothing */                           { [] } 

actual_list:
  | expr COMMA actual_list                  { $1::$3 }
  | expr                                    { [$1] } 


/* operator rules */
asn_op:
  | ASSIGN                                  { Asn } 
  | ADD_ASN                                 { AddAsn }
  | SUB_ASN                                 { SubAsn }
  | MUL_ASN                                 { MulAsn }
  | DIV_ASN                                 { DivAsn }
  | MOD_ASN                                 { ModAsn }
  | EXP_ASN                                 { ExpAsn }

eq_op:
  | EQ                                      { Eq }
  | NEQ                                     { Neq }

relat_op:
  | LT                                      { Lt }
  | GT                                      { Gt }
  | LEQ                                     { Leq }
  | GEQ                                     { Geq }

add_op:
  | PLUS                                    { Add }
  | MINUS                                   { Sub }

mult_op:
  | TIMES                                   { Mul }
  | DIVIDE                                  { Div }
  | MOD                                     { Mod }
  | EXPONENT                                { Exp }

prefix_op:
  | PLUS                                    { Pos } /* can't return nothing */
  | MINUS                                   { Neg }
  | LOG_NOT                                 { LogNot } 

fn_op:
  | MAP                                     { Map }
  | FILTER                                  { Filter }

iter_type:
  | FOR                                     { For }
  | DO                                      { Do }


/* type rules */
val_decl:
  | typ ID                                  { Bind(Immutable, $1, $2) }

typ:
  | typ LBRACK RBRACK                       { Array($1) }
  | unit_t                                  { $1 }

unit_t:
  | STRUCT ID                               { Struct($2) } /* user defined structs */
  | primitive_t                             { $1 }

primitive_t:
  | INT_T                                   { Int }
  | FLOAT_T                                 { Float }
  | STRING_T                                { String }
  | BOOL_T                                  { Bool }
  | vector_t                                { $1 }

vector_t:
  | VECTOR_T LT INT_LIT GT                  { Vector($3) }

fn_type:
  | GN                                      { Kn }
  | KN                                      { Gn }

decl_mod:
  | VAR                                     { Mutable }
  | /* nothing, val */                      { Immutable }


/* literal syntax rules */
lit:
  | struct_lit                              { $1 } 
  | array_lit                               { $1 }
  | vector_lit                              { $1 }
  | STRING_LIT                              { LitStr($1) }
  | BOOL_LIT                                { LitBool($1) }
  | FLOAT_LIT                               { LitFloat($1) }
  | INT_LIT                                 { LitInt($1) }

struct_lit:  
  | LBRACE struct_lit_fields RBRACE         { LitStruct($2) }
  | LBRACE RBRACE                           { LitStruct([]) }

struct_lit_fields:
  | struct_lit_field SEMI struct_lit_fields { $1::$3 }
  | struct_lit_field                        { [$1] }

struct_lit_field:
  | DOT ID ASSIGN expr                      { StructField($2, $4) }

array_lit:
  | LBRACK lit_elements RBRACK              { LitArray($2) }
  | LBRACK RBRACK                           { LitArray([]) }

vector_lit:
  | LPAREN lit_elements COMMA expr RPAREN   { LitVector($4::$2) }

lit_elements:
  | lit_elements COMMA expr                 { $3::$1 }
  | expr                                    { [$1] }


/* Type wrappers */
int_lit:
  | INT_LIT                                 { LitInt($1) }

id:
  | ID                                      { Id($1) }
