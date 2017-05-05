type ctyp = Sast.styp

type canon_val = ctyp

type cbind = Sast.sbind

type cbin_op =
  | CBinopInt of Sast.sbin_op_i
  | CBinopFloat of Sast.sbin_op_f
  | CBinopBool of Sast.sbin_op_b
  | CBinopPtr of Sast.sbin_op_p
  | BinopDud

type cun_op = Sast.sun_op

type clit =
  | CLitInt of int
  | CLitFloat of float
  | CLitBool of bool
  | CLitStr of string
  | CLitArray of cexpr list
  | CLitStruct of (string * cexpr) list
  | LitDud

and cexpr =
  | CLit of ctyp * clit
  | CId of ctyp * string
  | CBinop of ctyp * cexpr * cbin_op * cexpr
  | CAccess of ctyp * cexpr * string
  | CAssign of ctyp * cexpr * cexpr
  | CVal of ctyp * cexpr
  | CCall of ctyp * string * cstmt list
  | CUnop of ctyp * cun_op * cexpr
  | CCond of ctyp * cexpr * cexpr * cexpr
  | ExprDud

and cstmt =
  | CExpr of ctyp * cexpr
  | CBlock of ctyp * cstmt list
  | CLoop of ctyp * cexpr (* int *) * cstmt
  | CReturn of ctyp * cstmt
  | StmtDud

type cfn_decl = {
  cfname      : string;
  cret_typ    : ctyp;
  cformals    : cbind list;
  clocals     : cbind list;
  cbody       : cstmt list;
}

type cstruct_def = Sast.sstruct_def

type cdecl =
  | CFnDecl of cfn_decl
  | CStructDef of cstruct_def
  | CConstDecl of cbind * cstmt
  | CExternDecl of Sast.sextern_decl
  | DeclDud

type cprogram = cdecl list
