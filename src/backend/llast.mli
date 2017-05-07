type lltyp =
  | LLBool (* i1 *)
  | LLInt (* i32 *)
  | LLFloat (* double_type *)
  | LLConstString (* name and content, only used for representing strings, simply i8* *)
  | LLArray of lltyp * int
  | LLStruct of string
  | LLVoid (* only used for declaring function types *)

(* might not be used
type llbind = LLBind of lltyp * string
 *)

type lllit =
  | LLLitBool of bool
  | LLLitInt of int
  | LLLitFloat of float
  | LLLitString of string
  | LLLitStruct of string * lllit list

type llglobal = lltyp * string * lllit
type llstruct_def = string * lltyp list

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
  | LLBlockReturnVoid
  | LLBlockBr of llreg * string * string (* 1st of boolean, 2nd and 3 of label type *)
  | LLBlockJmp of string (* register must be a branch label type *)

type llstmt =
  | LLBuildBinOp of llops_typ * llreg * llreg * llreg
  | LLBuildCall of string * llreg list * llreg option(* 1 storing func def, 2 storing list of formals, 3 storing retval *)
  | LLBuildPrintCall of llreg (* register storing the integer that you want to print *)
  | LLBuildArrayLoad of llreg * llreg * llreg (* 1 has the arr label, 2 has index, 3 has dest label *)
  | LLBuildArrayStore of llreg * llreg * llreg (* 1 has the arr label, 2 has index, 3 has source label *)
  | LLBuildStructLoad of llreg * int * llreg (* 1 has the struct label, 2 has the index, 3 has dest label *)
  | LLBuildStructStore of llreg * int * llreg (* 1 has the struct label, 2 has the index, 3 has source label *)
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
