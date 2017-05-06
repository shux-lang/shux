open Ast
open Sast
open Semant

type svar = {
  id : string;
  scope : Sast.sscope;
  svar_type : Sast.styp;
}

(* keep track of names and types *) 
type strans_env = {
   variables : svar list VarMap.t;
   sfn_decl : sfn_decl VarMap.t;
   sstruct_map : sstruct_def VarMap.t
}

let rec to_styp senv = function
    | Some x -> (match x with
        | Int -> SInt
        | Float -> SFloat
        | String -> SString
        | Bool -> SBool
        | Vector(i) -> SArray(SFloat, Some i)
        | Struct(s) -> let sstruct_binds = (VarMap.find s senv.sstruct_map).ssfields 
                                           in SStruct(s, sstruct_binds)
        | Array(t,i) -> let n_styp = to_styp senv (Some t) in
                        SArray(n_styp, i)
        | Ptr -> SPtr
        | Void -> SVoid)
    | None -> SVoid

    (* iorf: true for int, false for float *) 
and to_sbin_op iorf = function
  | Add -> if iorf then SBinopInt SAddi else SBinopFloat SAddf 
  | Sub -> if iorf then SBinopInt SSubi else SBinopFloat SSubf
  | Mul -> if iorf then SBinopInt SMuli else SBinopFloat SMulf
  | Div -> if iorf then SBinopInt SDivi else SBinopFloat SDivf
  | Exp -> if iorf then SBinopInt SExpi else SBinopFloat SExpf
  | Eq  -> if iorf then SBinopInt SEqi else SBinopFloat SEqf
  | Lt  -> if iorf then SBinopInt SLti else SBinopFloat SLtf
  | Gt  -> if iorf then SBinopInt SGeqi else SBinopFloat SGeqf
  | Neq -> if iorf then SBinopInt SNeqi else SBinopFloat SNeqf
  | Leq -> if iorf then SBinopInt SLeqi else SBinopFloat SLeqf
  | Geq -> if iorf then SBinopInt SGeqi else SBinopFloat SGeqf
  | Mod -> SBinopInt SMod
  | LogAnd -> SBinopBool SLogAnd
  | LogOr -> SBinopBool SLogOr
  | Filter -> SBinopFn SFilter
  | Map -> SBinopFn SMap
  | For -> SBinopFn SFor
  | Do -> SBinopFn SDo
  | Index -> SBinopPtr SIndex

and get_styp_from_sexpr = function
    | SLit(s,_) -> s
    | SId(s,_,_) -> s
    | SLookback(s,_,_) -> s
    | SAccess(s,_,_) -> s
    | SBinop(s,_,_,_) -> s
    | SAssign(s,_,_) -> s
    | SKnCall(s,_,_) -> s
    | SGnCall(s,_,_) -> s
    | SLookbackDefault(s,_,_,_) -> s
    | SUnop(s,_,_) -> s
    | SCond(s,_,_,_) -> s

and to_slit senv = function
    | LitInt(i) -> SLitInt(i)
    | LitFloat(f) -> SLitFloat(f)
    | LitBool(b) -> SLitBool(b)
    | LitStr(s) -> SLitStr(s)
    | LitKn(l) -> SLitKn(to_slambda l)
    | LitVector(el) -> SLitArray(List.map (get_sexpr senv) el)
    | LitArray(e) -> SLitArray(List.map (get_sexpr senv) e)
    | LitStruct(s,e) -> SLitStruct("aa", []) (*TODO: *) 

and slit_to_styp = function
    | SLitInt(i) -> SInt
    | SLitFloat(f) -> SFloat
    | SLitBool(b) -> SBool
    | SLitStr(s) -> SString
    | SLitKn(l) -> l.slret_typ
    | SLitArray(elist) -> get_styp_from_sexpr (List.hd elist)
    | SLitStruct(name, slist) -> SStruct(name, []) (*TODO: Struct literal type translation *) 

(*TODO: *)
and to_slambda l = 
    { slret_typ = SInt; slformals = []; sllocals = []; slbody = [];
      slret_expr = (SLit(SInt, SLitInt(1)), SInt)} 

and mut_to_scope = function 
    | Mutable -> SLocalVar
    | Immutable -> SLocalVal

and to_sbind env = function
    | Bind(m, t, s) -> SBind(to_styp env (Some t), s, mut_to_scope m)

and translate_extern env ext
    = { sxalias = ext.xalias; sxfname = ext.xfname; 
        sxret_typ = to_styp env ext.xret_typ; sxformals = List.map (to_sbind env) ext.xformals }

and translate_struct_defs env struct_def = 
    {ssname  = struct_def.sname; ssfields = List.map (to_sbind env) struct_def.fields }

(*TODO: how the fuck is this going to work ayy lmao *) 
and get_sexpr senv = function
    | Lit(a) -> let sliteral = to_slit senv a in SLit(slit_to_styp sliteral, sliteral)
    | Id(s) -> let v = List.hd (VarMap.find s senv.variables)
               in SId(v.svar_type, s, v.scope)
    | Lookback(str, i) -> SLookback(SInt, str, i)
    | Binop(e1, bin_op, e2) -> 
        let st1 = get_sexpr senv e1 in 
        let n_binop = (match (get_styp_from_sexpr st1) with
        | SInt -> to_sbin_op true bin_op
        | SFloat -> to_sbin_op false bin_op) in 
        SBinop((get_styp_from_sexpr st1), st1, n_binop, get_sexpr senv e2)
    | _ -> raise (Failure "u sux") 


and translate_letdecl senv = function
    | LetDecl(b,e) -> SLetDecl(to_sbind senv b, get_sexpr senv e)
    | StructDef(s) -> SStructDef(translate_struct_defs senv s)
    | ExternDecl(e) -> SExternDecl(translate_extern senv e)

let if_letdecls = function
    | LetDecl(b, e) -> true
    | _ -> false

let translate_to_sast ((ns, globals, functions), env)= 
    let let_decls = List.map (translate_letdecl env) globals in 
    (ns, globals, functions)
