(* we can just expand typ to include void natively *)
type styp =
  | SInt
  | SFloat
  | SString
  | SBool
  | SStruct of string
  | SArray of styp
  | Void 

type sbind = SBind of styp * string

type sfn_typ = 
  | SKn | SGn

type sbin_op_i =
  | SAddi | SSubi | SMuli | SDivi | SMod | SExpi
  | SEqi | SLti | SNeqi | SLeqi | SGeqi

type sbin_op_f =
  | SAddf | SSubf | SMulf | SDivf | SExpf
  | SEqf | SLtf | SNeqf | SLeqf | SGeqf

type sbin_op_b =
  | SLogAnd | SLogOr

type sbin_op_p =
  | SIndex | SAccess
  | SAsn (* this is redundant because of the thing below *)

type sbin_op_fn =
  | SFilter | SMap
  | SFor | SDo
  | SLookback

type sbin_op = 
  | SBinopInt of sbin_op_i
  | SBinopFloat of sbin_op_f
  | SBinopBool of sbin_op_b
  | SBinopPtr of sbin_op_p
  | SBinopFn of sbin_op_fn

type sun_op = 
  | SLogNot | SNegi | SNegf

type slambda = {
  slformals   : sbind list;
  slbody      : sexpr list;
  sllocals    : sbind list; (* no lookback, const-ness not enforced *)
  slret_expr  : sexpr;
}

and slit =
  | SLitInt of int
  | SLitFloat of float
  | SLitBool of bool
  | SLitStr of string
  | SLitKn of slambda
  | SLitVector of sexpr list
  | SLitArray of sexpr list 
  | SLitStruct of sstruct_field list

and sstruct_field = SStructField of string * sexpr

(* still refer back expr, but expose types *)
and sexpr =
  | SLit of styp * slit
  | SId of styp * string
  | SBinop of styp * sexpr * sbin_op * sexpr
  | SAssign of styp * sexpr * sexpr
  | SCall of styp * string * sexpr list
  | SLookbackDefault of styp * sexpr * sexpr
  | SUnop of styp * sun_op * sexpr
  | SCond of styp * sexpr * sexpr * sexpr

type sfn_decl = {
  sfname      : string;
  sfn_typ     : sfn_typ;
  sret_typ    : styp;
  sformals    : sbind list;
  slocalvars  : sbind list;         (* do not have lookback *)
  slocalvals  : (sbind * int) list; (* might have lookback *)
  sbody       : sexpr list;
  sret_expr   : sexpr;
}

type sstruct_def = {
  ssname      : string;
  ssfields    : sbind list;
}

type sextern_decl = {
	sxalias			: string;
  sxfname     : string;
  sxret_typ   : styp;
  sxformals   : sbind list;
}

type slet_decl =
  | SLetDecl of sbind * sexpr
  | SStructDef of sstruct_def
  | SExternDecl of sextern_decl
