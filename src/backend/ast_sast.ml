open Ast
open Sast
open Semant

let rec to_styp env = function
    | Some x -> (match x with
        | Int -> SInt
        | Float -> SFloat
        | String -> SString
        | Bool -> SBool
        | Vector(i) -> SArray(SFloat, Some i)
        | Struct(s) -> SStruct(s, get_struct_binds env s)
        | Array(t,i) -> let n_styp = to_styp env (Some t) in
                        SArray(n_styp, i)
        | Ptr -> SPtr
        | Void -> SVoid)
    | None -> SVoid

and get_struct_binds env name = 
    let s = VarMap.find name env.structs in
    let to_sbind field = 
        SBind(to_styp env (Some (snd field)), fst field, SStructField) in 
    List.map to_sbind s.fields

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
and get_sexpr t expr = 
    SLit(t, SLitInt 4)

and translate_letdecl env = function
    | LetDecl(b,e) -> SLetDecl(to_sbind env b, get_sexpr (to_styp env (Some (get_bind_typ b))) e)
    | StructDef(s) -> SStructDef(translate_struct_defs env s)
    | ExternDecl(e) -> SExternDecl(translate_extern env e)

let if_letdecls = function
    | LetDecl(b, e) -> true
    | _ -> false

let translate_to_sast ((ns, globals, functions), env)= 
    let let_decls = List.map (translate_letdecl env) globals in 
    (ns, globals, functions)
