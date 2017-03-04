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

let_decl:
| LET type ID ASSIGN EXPRESSION SEMICOLON { }





