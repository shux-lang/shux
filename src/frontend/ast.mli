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

type bind =
  | Bind of typ * string

type fn_typ =
  | Kn
  | Gn

type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Asn | AddAsn | SubAsn | MulAsn | DivAsn | ModAsn | ExpAsn
  | Lt | Gt | Neq | Leq | Geq
  | And | Or
  | Filter | Map
  | Index | Lookback
  | For | Do

type un_op =
  | Not | Neg

type lambda = {
  formals   : bind list;
  body      : stmt list;
  ret_expr  : expr;
}

and lit =
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitStr of string
  | LitKn of lambda
  | LitVector of float list (* shouldn't we be able to construct vectors dynamically? *)
  | LitArray of expr list (* include optional type annotation here? *)
  | LitStruct of string * bind list (* should this be more sophisticated? *)

and expr =
  | NoExpr (* TODO: might this be a bad idea? *)
  | Lit of lit
  | Id of string
  | Binop of expr * bin_op * expr
  | Uniop of un_op * expr
  | Cond of expr * expr * expr (* technically Ternop *)
  | Call of string * expr list

and stmt =
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
  | LetDecl of bind * lit
  | StructDef of struct_def

type ns_def = {
  nname     : string;
  body      : program;
}

and ns_decl =
  | NsDecl of ns_def

and program = ns_decl list * let_decl list * fn_decl list
