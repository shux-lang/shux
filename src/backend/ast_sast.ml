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
        | Struct(s) -> SStruct(s, get_struct_binds s env)
        | Array(t,i) -> let n_styp = to_styp env (Some t) in
                        SArray(n_styp, i)
        | Ptr -> SPtr
        | Void -> SVoid)
    | None -> SVoid

and get_struct_binds name env = 
    let s = VarMap.find name env.structs in
    let to_sbind field = 
        SBind(to_styp env (Some (snd field)), fst field, SStructField) in 
    List.map to_sbind s.fields

and mut_to_scope = function 
    | Mutable -> SLocalVar
    | Immutable -> SLocalVal

and to_sbind env = function
    | Bind(m, t, s) -> SBind(to_styp env (Some t), s, mut_to_scope m)

and translate_extern ext env
    = { sxalias = ext.xalias; sxfname = ext.xfname; 
        sxret_typ = to_styp env ext.xret_typ; sxformals = List.map (to_sbind env) ext.xformals }

let translate_to_sast ast = 
   (fst ast)
