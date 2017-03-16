open Core.Std

type type_name = string

type typ =
  | Int
  | Float
  | String
  | Bool
  | Struct of type_name
  | Array of typ
  | Vector of int

type fn_typ =
  | Kn
  | Gn

type binary_operator =
  | Add | Sub | Mul | Div | Mod | Exp
  | Asn | AddAsn | SubAsn | MulAsn | DivAsn | ModAsn | ExpAsn
  | Lt | Gt | Neq | Leq | Geq
  | And | Or
  | Filter | Map
  | Index | Dot | DotDot

type unary_operator =
  | Not | Neg

type iter_typ =
  | For
  | Do

type lambda = {
  formals   : bind list;
  body      : stmt list;
  ret_expr  : expr;
}
  
type expr =
  | Lit of lit
  | Lambda of lambda
  | Id of string
  | Binop of expr * bin_op * expr
  | Uniop of un_op * expr
  | Cond of expr * expr * expr
  | Iter of iter_typ * expr * expr
  | Call of string * expr list

type stmt =
  | VDecl of bind * expr
  | Expr of expr

type fn_decl = {
  fname     : string;
  fn_typ    : fn_typ;
  ret_typ   : typ;
  formals   : bind list;
  body      : stmt list;
  ret_expr  : expr;
}

type struct_def = {
  sname     : string;
  fields    : bind list;
}

type let_decl =
  | LetDecl of bind
  | StructDef of struct_def

type ns_def = {
  nname     : string;
  body      : program;
}

type ns_decl =
  | NsDecl of ns_def

type program = ns_decl list * let_decl list * fn_decl list
