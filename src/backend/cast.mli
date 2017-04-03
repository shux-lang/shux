
type typ =
  | Void
  | Int
  | Float
  | String
  | Bool
  | Array of typ (* Vectors are now just Array(Float) *)
  | Struct of string
 (* still need to figure out what to do for Structs *)

type mut =
  | Mutable
  | Immutable

type bind = mut * typ * string

type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Asn | Eq | Lt | Gt | Neq | Leq | Geq
  | LogAnd | LogOr | Access

type un_op =
  | LogNot | Neg

type lit =
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitStr of string
  | LitArray of expr list
  | LitStruct of struct_field list

and struct_field = StructField of string * expr

and expr =
  | Lit of lit
  | Binop of expr * bin_op * expr
  | Call of string * expr list
  | Id of string
  | Uniop of un_op * expr
  | Cond of expr * expr * expr
  | Assign of expr * expr

type stmt = 
  | Expr of expr
  | For of int * expr * expr list
  | Return of expr

type fn_decl = {
  rname       : string;
  rret_type   : typ;
  rformals    : bind list;
  rlocals     : bind list;
  rbody       : stmt list;
}

type struct_def = {
  sname       : string;
  fields      : bind list;
}

type static_decl =
  | Lambda of fn_decl
  | StaticString of string
  | StructDef of string * bind list
  | ValDef of bind
  | ExternDecl of string * typ * bind list

type ll_program = static_decl list * fn_decl list
