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
  | LLRegLit of lltyp * lllit

(* might not be used
type llacc_typ =
  | LLBuildLoad
  | LLBuildStore
  | LLBuildAlloc
 *)

type llops_typ =
  | LLAdd
(*
  | LLFAdd
  | LLSub
  | LLFSub
  | LLMul
  | LLFMul
  | LLDiv (* signed *)
  | LLFDiv
 *)
  | LLLT (* integer less than *)
(*
  | LLFLT
  | LLEQ
  | LLFEQ
 *)
(* many left*)

type llblock_term = (* a block must be terminated by either a jump or a return *)
  | LLBlockReturn of llreg
  | LLBlockBr of llreg * llreg * llreg (* 1st of boolean, 2nd and 3 of label type *)
  | LLBlockJmp of llreg (* register must be a branch label type *)

type llstmt =
  | LLBuildBinOp of llops_typ * llreg * llreg * llreg
  | LLBuildAlloc of llreg * lltyp
  | LLBuildStore of llreg * llreg (* store from an actual register to a pointer typed register*)
  | LLBuildLoad of llreg * llreg (* load from left ptr register to right actual register*)
  | LLBuildCall of string * llreg list * llreg (* 1 storing func def, 2 storing list of formals, 3 storing retval *)
  | LLBuildPrintCall of llreg (* register storing the integer that you want to print *)
  | LLBuildTerm of llblock_term


type llblock = {
    llbname : string;
    llbbody : llstmt list;
}

type llfunc_def = {
    llfname : string;
    llfformals : llreg list;
    llflocals : llreg list;
    llfbody : llstmt list;
    llfreturn : lltyp;
    llfblocks : llblock list;
}

type llprog = llfunc_def list
