type lltyp =
  | LLBool (* i1 *)
  | LLInt (* i32 *)
  | LLFloat (* double_type *)
  | LLConstString of string (* name and content, only used for representing strings, simply i8* *)
  | LLArray of lltyp list * int
  | LLStruct of string * lltyp list
  | LLVoid (* only used for declaring function types *)
  | LLBlock
(* | LLVarFunc of lltyp * lltyp list  only used for printf *)

(* might not be used
type llbind = LLBind of lltyp * string
 *)

type lllit =
  | LLLitBool of bool
  | LLLitInt of int
  | LLLitFloat of float
  | LLLitString of string

type llreg =
  | LLRegLabel of lltyp * string(* register can store a name and an lltyp value *)
  | LLRegLit of lllit

(* might not be used
type llacc_typ =
  | LLBuildLoad
  | LLBuildStore
  | LLBuildAlloc
 *)

type llops_typ =
  | LLBuildAdd of lltyp * lltyp
(*
  | LLBuildFAdd
  | LLBuildSub
  | LLBuildFSub
  | LLBuildMul
  | LLBuildFMul
  | LLBuildDiv (* signed *)
  | LLBuildFDiv
 *)

type llcmp_typ =
  | LLIEQ
  | LLFEQ
  | LLILT
  | LLFLT
(* many left*)

type llstmt =
  | LLBuildCmp of llcmp_typ * lltyp * lltyp
  | LLBuildOp of llops_typ * lltyp * lltyp
  | LLBuildAlloc of llreg * lltyp
  | LLBuildStore of llreg * llreg (* store from an actual register to a pointer typed register*)
  | LLBuildLoad of llreg * llreg (* load from left ptr register to right actual register*)
  | LLBuildCall of llreg * llreg list * llreg (* 1 storing func def, 2 storing list of formals, 3 storing retval *)
  | LLBuildPrintCall of llreg (* register storing the integer that you want to print *)

type llblock_term = (* a block must be terminated by either a jump or a return *)
  | LLBlockReturn of llreg
  | LLBlockBr of llreg * llreg * llreg (* 1st of boolean, 2nd and 3 of label type *)
  | LLBlockJmp of llreg (* register must be a branch label type *)

type llblock = {
    llbname : string;
    llbbody : llstmt list;
    llbterm  : llblock_term; (* string indicating which block to jump to *)
}

type llfunc_def = {
    llfname : string;
    llfformals : lltyp list;
    (*llfreturn : lltyp;*)
    llflocals : lltyp list;
    llfbody : llblock list;
}

type llprog = llfunc_def list

