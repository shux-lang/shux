module L = Llvm
open Llast

module StringMap = Map.Make(String)

let translate funcs =
  let the_context = L.global_context () in
  let the_module = L.create_module the_context "shux" and
      i32_t = L.i32_type the_context and
      i8_t = L.i8_type the_context and
      i1_t = L.i1_type the_context and
      float_t = L.float_type the_context
  in
  let str_t = L.pointer_type i8_t in

  let extract_type = function
      LLRegLabel (typ, str) -> typ
    | LLRegLit (typ, lit) -> typ in

  let lltyp_of = function
      LLBool -> i1_t
    | LLInt  -> i32_t
    | LLFloat -> L.double_type the_context
    | _ -> i32_t
  in

  let combine lit = lltyp_of (extract_type lit) in

  (* Define the printf function *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let define_funcs =
    let translate_func map func=
      let fname = func.llfname and
          fformals = Array.of_list (List.map combine (func.llfformals))
      in
      let func_sign = L.function_type (lltyp_of func.llfreturn) fformals in
      let func_def = L.define_function fname func_sign the_module in
      StringMap.add fname (func_def, func) map
    in
    List.fold_left translate_func StringMap.empty funcs in

  let get_func_by_name fname =
    let func_llvalue, _ = StringMap.find fname define_funcs in
    func_llvalue
  in

  let build_funcs=
    let build_func func =
      let (the_function, _) = StringMap.find func.llfname define_funcs in
      let builder = L.builder_at_end the_context (L.entry_block the_function) in
      let int_format_str = L.build_global_stringptr "%d\n%" "fmtint" builder in
      let formals_list = Array.to_list (L.params the_function) in

      let get_reg_typ_name = function
          LLRegLabel (typ, str) -> (typ, str)
        | LLRegLit (typ,lit) -> (typ,"nonameliteral") in

      let build_formals = (* this is a map from formal name to its stack ptr *)
        let build_formal map formal_def formal_param =
          let (formal_type,formal_name) = get_reg_typ_name formal_def in
          let formal_ptr = L.build_alloca (lltyp_of formal_type) formal_name builder in
          ignore(L.build_store formal_param formal_ptr builder);
          StringMap.add formal_name formal_ptr map in
        List.fold_left2 build_formal StringMap.empty func.llfformals formals_list
      in

      let build_locals = (* this is a map from local name to its stack ptr *)
        let build_local map local_def =
          let (local_type, local_name) = get_reg_typ_name local_def in
          let local_ptr = L.build_alloca (lltyp_of local_type) local_name builder in
          StringMap.add local_name local_ptr map in
        List.fold_left build_local StringMap.empty func.llflocals
      in

      let define_blocks =
        let define_block map block_def =
          let block_name = block_def.llbname and block_stmts = block_def.llbbody in
          let block_llvalue = L.append_block the_context block_name the_function in
          StringMap.add block_name (block_stmts, block_llvalue) map in
        List.fold_left define_block StringMap.empty func.llfblocks
      in

      (* helper function starts here *)
      let llvalue_of_lit = function
          LLLitBool bool -> L.const_int i1_t (if bool then 1 else 0)
        | LLLitInt int -> L.const_int i32_t int
        | LLLitFloat float ->L.const_float float_t float
        | _ -> L.const_int i32_t 0
      in

      let get_reg = function
          LLRegLabel (typ, regname) ->
          if (StringMap.mem regname build_formals)
          then (StringMap.find regname build_formals)
          else (StringMap.find regname build_locals)
        | LLRegLit (typ, literal) ->
           let literal_ptr = L.build_alloca (lltyp_of typ) "lit_alloc_inst" builder in
           ignore(L.build_store (llvalue_of_lit literal) literal_ptr builder);
           literal_ptr
      in

      let load_reg ptrreglabel block_builder =
        L.build_load (get_reg ptrreglabel) "loadinst" block_builder
      in

      let store_reg ptrreglabel val_to_store block_builder=
        L.build_store val_to_store (get_reg ptrreglabel) block_builder in

      let make_tuple list element =
        let bundle a = (a,element) in
        List.map bundle list
      in

      let transform_funclabel_list_to_llvalue label_list block_builder =
        let builder_list = List.map (fun _ -> block_builder) label_list in
        let result = List.map2 load_reg label_list builder_list in
        Array.of_list result in

      (* helper functions end here *)

      let build_terminator block_builder = function
          LLBlockReturn label -> L.build_ret (load_reg label block_builder) block_builder
        | LLBlockBr (a,b,c) -> L.const_int i32_t 0
        | LLBlockJmp a -> L.const_int i32_t 0 in


      let bind_block_to_stmt stmts = function
          "entry" -> make_tuple stmts builder
        | bname -> let (_,block_llvalue) = StringMap.find bname define_blocks in
                  make_tuple stmts (L.builder_at_end the_context block_llvalue)
      in

      let build_stmt map block_stmt=
        let (stmt, block_builder) = block_stmt in
        let stmt_llvalue =
          (match stmt with
             LLBuildPrintCall label
             -> L.build_call printf_func
                             [| int_format_str; (load_reg label block_builder) |] "printwhatever" block_builder
           | LLBuildCall (fname,label_list,labelret) ->
              let func_llvalue = get_func_by_name fname and
                  func_formal_array_llvalue = transform_funclabel_list_to_llvalue label_list block_builder in
              let call_ret_reg = L.build_call func_llvalue func_formal_array_llvalue "funccall" block_builder in
                  ignore(store_reg labelret call_ret_reg block_builder);
                  call_ret_reg
           | LLBuildBinOp (optyp,label1,label2,labelresult) ->
              let reg1 = load_reg label1 block_builder and reg2 = load_reg label2 block_builder in
              let regbinop = (match optyp with
                                LLAdd -> L.build_add reg1 reg2 "addinst" block_builder
                              | LLLT -> L.build_icmp L.Icmp.Slt reg1 reg2 "ltinst" block_builder
                             ) in
              store_reg labelresult regbinop block_builder
           | LLBuildTerm terminator -> build_terminator block_builder terminator
           | _ -> L.const_int i32_t 0
          ) in
        StringMap.add ("stmt"^func.llfname) stmt_llvalue map
      in
      let bundled_stmts = bind_block_to_stmt func.llfbody "entry" in
      ignore(List.fold_left build_stmt StringMap.empty bundled_stmts); (*build the entry block*)
      (* now let's build each block *)
      let build_blocks =
        let build_block block =
          let bname = block.llbname in
          let bundled_stmts = bind_block_to_stmt block.llbbody bname in
          ignore(List.fold_left build_stmt StringMap.empty bundled_stmts);
          ()
        in
        List.iter build_block func.llfblocks
      in
      let _ = build_blocks in
      () (* end of build_funcs *)
    in
    List.iter build_func funcs in

  let _ = Llvm_analysis.assert_valid_module the_module in
  the_module
