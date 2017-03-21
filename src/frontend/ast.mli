open Core.Std

type typ =
  | Int
  | Float
  | String
  | Bool
  | Struct of string
  | Array of typ
  | Vector of int

type mut =
  | Mutable
  | Immutable

type bind = Bind of mut * typ * string

type fn_typ =
  | Kn
  | Gn

type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Asn | AddAsn | SubAsn | MulAsn | DivAsn | ModAsn | ExpAsn
  | Eq | Lt | Gt | Neq | Leq | Geq
  | LogAnd | LogOr
  | Filter | Map
  | Index | Lookback | StructField
  | For | Do
  | Call

type un_op =
  | LogNot | Neg | Pos

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
  | LitStruct of struct_field list (* should this be more sophisticated? *)

and struct_field = StructField of string * expr

and expr =
  | NoExpr (* TODO: might this be a bad idea? *)
  | Lit of lit
  | Id of string
  | Binop of expr * bin_op * expr
  | Uniop of un_op * expr
  | Cond of expr * expr * expr (* technically Ternop *)

and stmt =
  | VDecl of bind * expr option
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
