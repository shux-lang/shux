(* we can just expand typ to include void natively *)
type styp =
 | SInt
 | SFloat
 | SString
 | SBool
 | SStruct of string
 | SArray of styp
 | Void 

type sbind  = SBind of styp * string

type sfn_typ = 
 | SKn
 | SGn

type sbin_op = 
 | SAddi | SSubi | SMuli | SDivi | SMod | SExpi
 | SAddf | SSubf | SMulf | SDivf | SExpf
 | SAsn
 | SEqi | SLti | SNeqi | SLeqi | SGeqi
 | SEqf | SLtf | SNeqf | SLeqf | SGeqf
 | SLogAnd | SLogOr
 | SFilter | SMap
 | SIndex | SLookback
 | SFor | SDo
 | SAccess

type sun_op = 
 | SLogNot | SNegi | SNegf | SPosi | SPosf

type slambda = {
  slformals  : sbind list;
  slbody     : sstmt list;
  slret_expr : sexpr;
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
  | SUniop of styp * sun_op * sexpr
  | SCond of styp * sexpr * sexpr * sexpr

and sstmt =
  | SVDecl of sbind * sexpr
  | SExpr of sexpr

type sfn_decl = {
  sfname     : string;
  sfn_typ    : sfn_typ;
  sret_typ   : styp;
  sformals   : sbind list;
  sbody      : sstmt list;
  sret_expr  : sexpr;
}

type sstruct_def = {
	sname			:	string;
	sfields		: sbind list;
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

and sprogram = slet_decl list * sfn_decl list 
