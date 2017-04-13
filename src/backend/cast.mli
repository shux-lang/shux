(* cast.ml *) 
(* Makes most sense to implement this ground-up *) 

(* I am also going to use just regular names...
   unsure how OCaml handles name type conflicts *) 

(* Generator *) 
(*
type buffer = typ * int

type gn_struct = buffer list

type gn = Gn of gn_struct * kn
*)

type ctyp = Sast.styp

type cbind = Sast.sbind

type cbin_op =
  | CBinopInt of Sast.sbin_op_i
  | CBinopFloat of Sast.sbin_op_f
  | CBinopBool of Sast.sbin_op_b
  | CBinopPtr of Sast.sbin_op_p

type cun_op = Sast.sun_op

type clit =
  | CLitInt of int
  | CLitFloat of float
  | CLitBool of bool
  | CLitStr of string
  | CLitArray of cexpr list
  | CLitStruct of cstruct_field list

and cstruct_field = CStructField of string * cexpr

and cexpr =
  | CLit of ctyp * clit
  | CId of ctyp * string
  | CBinop of ctyp * cexpr * cbin_op * cexpr
  | CAssign of ctyp * cexpr * cexpr
  | CCall of ctyp * string * cexpr list
  | CUnop of ctyp * cun_op * cexpr
  | CCond of ctyp * cexpr * cexpr * cexpr

type cfn_decl = Sast.sfn_decl

type cstruct_def = Sast.sstruct_def

type cdecl =
  | CFnDecl of cfn_decl
  | CStructDef of cstruct_def
  | CConstDecl of cbind * cexpr
  | CExternDecl of Sast.sextern_decl

type cprogram = cdecl list
