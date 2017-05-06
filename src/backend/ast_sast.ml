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
    | LitKn(l) -> SLitKn(to_slambda senv l)
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

and to_sunop iorf = function
    | LogNot -> SLogNot
    | Neg -> if iorf then SNegi else SNegf
    | Pos -> raise (Failure "Pos isnt supposed to exist in SAST") 

(*TODO: *)
and to_slambda senv l = 
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
        let st1 = get_sexpr senv e1 in (match bin_op with
        | Add | Sub | Mul | Div | Mod | Exp | Eq | Lt | Gt | Neq | Leq | Geq -> 
            let sbinop = (match get_styp_from_sexpr st1 with
                  | SInt -> to_sbin_op true bin_op
                  | SFloat -> to_sbin_op false bin_op
                  | _ -> raise (Failure "Not Integer/Float type on binop")) in 
            SBinop(get_styp_from_sexpr st1, st1, sbinop, get_sexpr senv e2)
        | LogAnd | LogOr -> SBinop(SBool, st1, to_sbin_op true bin_op, get_sexpr senv e2)
        | Filter -> SBinop(SArray(get_styp_from_sexpr st1, None), st1, SBinopFn SFilter, get_sexpr senv e2)
        | Map -> SBinop(SArray(get_styp_from_sexpr (get_sexpr senv e2), None),
                               st1, SBinopFn SMap, get_sexpr senv e2)
        | Index -> SBinop(get_styp_from_sexpr st1, st1, SBinopPtr SIndex, get_sexpr senv e2)
        | For -> SBinop(SArray(get_styp_from_sexpr (get_sexpr senv e2), None), st1, SBinopFn SFor, get_sexpr senv e2)
        | Do -> SBinop(get_styp_from_sexpr (get_sexpr senv e2), st1, SBinopFn SDo, get_sexpr senv e2))
    | Assign(e1, e2) -> let st1 = get_sexpr senv e1 in 
                        SAssign(get_styp_from_sexpr st1, st1, get_sexpr senv e2)
    | Call(s, elist) -> (match s with
        | Some s -> let f = VarMap.find s senv.sfn_decl in (match f with
            | SGnDecl(gn) -> SGnCall(gn.sgret_typ, s, List.map (get_sexpr senv) elist)
            | SKnDecl(kn) -> SKnCall(kn.skret_typ, s, List.map (get_sexpr senv) elist))
        | None -> SGnCall(SInt, "_", []))
    | Uniop(u, e) -> (match u with
        | LogNot -> let st1 = get_sexpr senv e in 
                    SUnop(get_styp_from_sexpr st1, to_sunop true u, st1)
        | Neg -> let st1 = get_sexpr senv e in 
                 let sunop = (match get_styp_from_sexpr st1 with
                     | SInt -> to_sunop true u
                     | SFloat -> to_sunop false u
                     | _ -> raise (Failure "Not Integer/Float type on unnop"))
                 in SUnop(get_styp_from_sexpr st1, sunop, st1)
        | Pos -> get_sexpr senv e)
    | LookbackDefault(e1, e2) -> 
                    let se1 = get_sexpr senv e1
                    and se2 = get_sexpr senv e2
                    and i = (match e1 with
        | Lookback(str, i) -> i
        | _ -> raise (Failure "LookbackDefault not preceded by Lookback expression"))
        in SLookbackDefault(get_styp_from_sexpr se1, i, se1, se2)
    | Cond(e1, e2, e3) -> 
                    let se1 = get_sexpr senv e1
                    and se2 = get_sexpr senv e2
                    and se3 = get_sexpr senv e3
                    in SCond(get_styp_from_sexpr se2, se1, se2, se3)
    | Access(e, str) -> let se = get_sexpr senv e in 
                        SAccess(get_styp_from_sexpr se, se, str)


and translate_letdecl senv = function
    | LetDecl(b,e) -> SLetDecl(to_sbind senv b, get_sexpr senv e)
    | StructDef(s) -> SStructDef(translate_struct_defs senv s)
    | ExternDecl(e) -> SExternDecl(translate_extern senv e)

let if_letdecls = function
    | LetDecl(b, e) -> true
    | _ -> false


let empty_senv = { variables = VarMap.empty; sfn_decl = VarMap.empty;
                   sstruct_map = VarMap.empty; }

let translate_to_sast (ns, globals, functions) = 
    let let_decls = List.map (translate_letdecl empty_senv) globals in 
    (ns, globals, functions)
