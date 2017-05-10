open Sast
open Cast
open Llast

let cast_to_llast cast =
  let rec ctyp_to_lltyp = function
      SInt -> LLInt
    | SFloat -> LLDouble
    | SBool -> LLBool
    | SArray (styp, int) -> LLArray (ctyp_to_lltyp styp, int)
    | SStruct (name, _) -> LLStruct name
    | _ -> LLInt
  in

  let rec translate_clit = function
      CLitInt i -> LLLitInt i
    | CLitFloat f -> LLLitDouble f
    | CLitBool b -> LLLitBool b
    | CLitStr s -> LLLitString s
    | _ -> LLLitInt 0 (*TODO fix this*)
  in


  let a_decl i = "a" ^ (string_of_int i) in
  let t_decl i = "t" ^ (string_of_int i) in

  let rec walk_cstmt a_stack t_stack cnt head llinsts = function
    | CExpr(t, e) -> walk_cexpr a_stack t_stack cnt head llinsts e
    | CPushAnon(t, s) ->
       let v = a_decl cnt in
       walk_cstmt (v::a_stack) t_stack (cnt + 1) (v::head) llinsts s
    | CReturn opt -> (match opt with
                        Some (typ, CPushAnon(t, cstmt)) when t=typ ->
                        let v = a_decl cnt in
                        let (cnt, head, r1, llinsts) =
                          walk_cstmt (v::a_stack) t_stack (cnt + 1) (v::head) llinsts cstmt
                        in
                        let llinst = LLBuildTerm (LLBlockReturn r1) in
                        (cnt, head, r1, llinst::llinsts)
                      | Some _ -> assert false
                      | None ->
                         let llinst = LLBuildTerm LLBlockReturnVoid and
                             llreg = LLRegLit (LLInt, (LLLitInt 0)) in
                         (cnt, head, llreg, llinst::llinsts)
                     )
    | _ -> assert false

  and walk_cexpr a_stack t_stack cnt head llinsts= function
      CLit (typ, lit) -> (cnt, head, LLRegLit (ctyp_to_lltyp typ,(translate_clit lit)), llinsts)
    | CId (typ, str) -> (cnt, head, LLRegLabel (ctyp_to_lltyp typ,str), llinsts)
    | CBinop (typ, expr1, op, expr2) ->
       let (cnt,head,e1, llinsts) = walk_cexpr a_stack t_stack cnt head llinsts expr1 in
       let (cnt,head,e2, llinsts) = walk_cexpr a_stack t_stack cnt head llinsts expr2 in
       let v = t_decl cnt in
       let (cnt, head, t_stack) = (cnt + 1, v::head, v::t_stack) in
       let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
       let llinst = LLBuildBinOp (LLAdd, e1, e2, vreg) in
       (cnt, head, vreg, llinst::llinsts)
    | CAssign (typ, expr1, expr2) -> assert false
    | CPeekAnon typ ->
       let anon = (match head with h::t -> h | [] -> assert false) in
       let anonreg = LLRegLabel (ctyp_to_lltyp typ, anon) in
       (cnt, head, anonreg, llinsts)
    | CPeek2Anon typ ->
       let anon = (List.nth head 1) in
       let anonreg = LLRegLabel (ctyp_to_lltyp typ, anon) in
       (cnt, head, anonreg, llinsts)
    | CPeek3Anon typ ->
       let anon = (List.nth head 2) in
       let anonreg = LLRegLabel (ctyp_to_lltyp typ, anon) in
       (cnt, head, anonreg, llinsts)
    | _ -> assert false
  in

  let statements_list_builder stmt =
    walk_cstmt [] [] 0 [] [] stmt in

  (* quiet *)
  let rec translate_cexpr = function
      CLit (typ, lit) -> (ctyp_to_lltyp typ, "CLit", LLBuildNoOp)
    | CId  (typ, str) -> (ctyp_to_lltyp typ, str, LLBuildNoOp)
    | _ ->  (LLInt, "hel", LLBuildNoOp)

  and translate_cstmt = function
      CReturn opt -> (match opt with
                        Some (typ, cstmt) -> LLBuildTerm (LLBlockReturn (LLRegLit (LLInt, LLLitInt 31)))
                      | None -> LLBuildTerm LLBlockReturnVoid
                     )
    | CExpr (typ, expr) -> LLBuildTerm LLBlockReturnVoid
    | CCond _ -> LLBuildTerm LLBlockReturnVoid
    | _ -> LLBuildPrintCall (LLRegLit (LLConstString, LLLitString "statement\n"))
  in

  let pretty_print_regs l =
    let str_typ = function
      | LLBool -> "bool"
      | LLInt -> "int"
      | LLConstString -> "string"
      | LLDouble -> "double"
      | _ -> "other"
    in
    let print_reg = function
        LLRegLabel (typ,str) -> ("Label-"^(str_typ typ)^"-"^str)
      | LLRegLit (typ, lit) -> ("Lit-"^(str_typ typ))
    in
    List.map print_reg l in

  let translate_cdecl list = function
    | CFnDecl cfunc ->
       let body = List.map translate_cstmt cfunc.cbody in
       let blocks = List.map statements_list_builder cfunc.cbody in
       let print_list l = List.iter print_string l in
       let extract_register = function
           (_,sl,_,_) -> print_list sl in
       ignore(List.iter extract_register blocks);
       let declare_fml_lcl (SBind (ctyp, cstr, _)) =
         LLRegLabel (ctyp_to_lltyp ctyp, cstr) in
       let formals = List.map declare_fml_lcl cfunc.cformals in
       let locals = List.map declare_fml_lcl cfunc.clocals in
       {llfname=cfunc.cfname;llfformals=formals;llflocals=locals;llfbody=body;llfreturn=
        if(cfunc.cret_typ = SVoid) then (LLVoid) else (ctyp_to_lltyp cfunc.cret_typ);llfblocks=[]}::list
    | _ -> list
  in

  (* quiet*)

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

  let define_globals =
    let define_global list = function
      | CConstDecl (SBind (ctyp, cname,_), clit) ->
         (ctyp_to_lltyp ctyp, cname, translate_clit clit) :: list
      | _ -> list
    in
    List.fold_left define_global [] cast
  in

  let translate_cprogram =
    List.fold_left translate_cdecl [] cast
  in

  (define_structs,define_globals,translate_cprogram) (*struct llglobal func*)
