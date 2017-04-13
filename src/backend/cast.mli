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

type ctyp = styp

type cbind = sbind

type cbin_op =
  | CAddi | CSubi | CMuli | CDivi | CMod | CExpi
  | CAddf | CSubf | CMulf | CDfvf | CMod | CExpf
  | CAsn
  | CEqi | CLti | CGti | CNeqi | CLeqi | Geqi
  | CEqf | CLtf | CGtf | CNeqf | CLeqf | Geqf
  | CLogAnd | CLogOr
  | CIndex
  | CAccess

type cun_op =
  | CLogNot | CNegi | CNegf

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
  | CBinop of ctyp * cexpr * cbin_op * cexper
  | 

type cdecl =
  | CFnDecl of cfn_decl
  | CStructDef of cstruct_def
  | CConstDecl of cbind * cexpr
  | SExternDecl of sextern_decl

type cprogram = cdecl list
