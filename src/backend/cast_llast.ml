open Sast
open Cast
open Llast

let cast_to_llast cast =
  let ctyp_to_lltyp = function
      SInt -> LLInt
    | SFloat -> LLDouble
    | SBool -> LLBool
    | _ -> LLInt (* TODO *)
  in

  let rec translate_clit = function
      CLitInt i -> LLLitInt i
    | _ -> LLLitInt 0
  in

  let rec translate_cexpr = function
      CLit (typ, lit) -> LLRegLit ((ctyp_to_lltyp typ), (translate_clit lit))
    | _ -> LLRegLit (LLInt, LLLitInt 0)
  in

  let rec translate_cstmt = function
      CReturn opt -> (match opt with
                        Some (typ, cstmt) -> LLBuildTerm (LLBlockReturn (LLRegLit (LLInt, LLLitInt 31)))
                      | None -> LLBuildTerm LLBlockReturnVoid
                     )
    | CExpr (typ, expr) -> LLBuildTerm LLBlockReturnVoid
    | CCond _ -> LLBuildTerm LLBlockReturnVoid
    | _ -> LLBuildPrintCall (LLRegLit (LLConstString, LLLitString "statement\n"))
  in

  let translate_cdecl list = function
    | CFnDecl cfunc ->
       let body = List.map translate_cstmt cfunc.cbody in
       let declare_fml_lcl (SBind (ctyp, cstr, _)) =
         LLRegLabel (ctyp_to_lltyp ctyp, cstr) in
       let formals = List.map declare_fml_lcl cfunc.cformals in
       let locals = List.map declare_fml_lcl cfunc.clocals in
         {llfname=cfunc.cfname;llfformals=formals;llflocals=locals;llfbody=body;llfreturn=LLInt;llfblocks=[]}::list
    | _ -> list
  in
  let translate_cprogram =
    List.fold_left translate_cdecl [] cast
  in

  let define_structs =
    let unbind (SBind (ctyp, _, _))=
      ctyp_to_lltyp ctyp
    in
    let define_struct list = function
      | CStructDef cstruct ->
         let struct_name = cstruct.ssname and
             field_list = cstruct.ssfields in
         (struct_name, List.map unbind field_list)::list
      | _ -> list
    in
    List.fold_left define_struct [] cast
  in
  (define_structs,[],translate_cprogram) (*struct llglobal func*)
