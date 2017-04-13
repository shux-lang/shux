open Ast

(* we can just expand typ to include void natively *)
type styp =
 | typ
 | Void 

type slambda = {
  lformals  : lformals;
  lbody     : lbody;
  lret_expr : sexpr;
}

and slit =
  | SLitInt of LitInt
  | SLitFloat of SLitFloat
  | SLitBool of LitBool
  | SLitStr of LitStr
  | SLitKn of slambda
  | SLitVector of sexpr list
  | SLitArray of sexpr list 
  | SLitStruct of SStruct_field list (* should this be more sophisticated? *)

and SStruct_field = SStructField of string * sexpr

(* still refer back expr, but expose types *)
and sexpr =
  | SLit of styp * lit
  | SId of styp * string
  | SBinop of styp * expr * bin_op * expr
  | SAssign of styp * expr * expr
  | SCall of styp * string * expr list (*I removed option here. What is "_" as a call? *)
  | SLookbackDefault of styp * expr * expr
  | SUniop of styp * un_op * expr
  | SCond of styp * expr * expr * expr

and sstmt =
  | VDecl of bind * sexpr
  | SExpr of sexpr

(* void (* kernel *) functions *)
type vfn_decl = {
  fname     : string;
  formals   : bind list;
  body      : stmt list;
  }

type sfn_decl = {
  fname     : string;
  fn_typ    : fn_typ;
  ret_typ   : styp;
  formals   : bind list;
  body      : stmt list;
  ret_expr  : sexpr;
}

type sextern_decl = {
  exfname     : string;
  exret_typ   : styp;
  exformals   : bind list;
}

type slet_decl =
  | SLetDecl of LetDecl
  | SStructDef of StructDef
  | SExternDecl of sextern_decl

and sprogram = ns_decl list * slet_decl list * sfn_decl list * vfn_decl list 
