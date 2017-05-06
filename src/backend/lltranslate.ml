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

  let lltype_of = function
      LLBool -> i1_t
    | LLInt  -> i32_t
    | LLFloat -> L.double_type the_context
    | _ -> i32_t
  in

  let lltype_of_lit = function
      LLLitBool x-> i1_t
    | LLLitInt  x-> i32_t
    | LLLitFloat x-> L.double_type the_context
    | _ -> i32_t
  in

  let combine lit = lltype_of (extract_type lit) in

  (* Define the printf function *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let declare_funcs =
    let translate_func map func=
      let fname = func.llfname and
          fformals = Array.of_list (List.map combine (func.llfformals))
      in
      let func_sign = L.function_type (lltype_of func.llfreturn) fformals in
      let func_def = L.define_function fname func_sign the_module in
      StringMap.add fname (func_def, func) map
    in
    List.fold_left translate_func StringMap.empty funcs in

  let build_funcs=
    let build_func func =
      let (the_function, _) = StringMap.find func.llfname declare_funcs in
      let builder = L.builder_at_end the_context (L.entry_block the_function) in
      let int_format_str = L.build_global_stringptr "%d\n%" "fmtint" builder in
      let formals_list = Array.to_list (L.params the_function) in

      let get_formal_name = function
          LLRegLabel (typ, str) -> str
        | _ -> "nonameliteral" in

      let build_formals = (* this is a map from formal name to formal register *)
        let build_formal map formal_def formal_param =
          let formal_name = (get_formal_name formal_def) in
          ignore(L.set_value_name formal_name formal_param);
          StringMap.add formal_name formal_param map in
        List.fold_left2 build_formal StringMap.empty func.llfformals formals_list
      in

      let llvalue_of_lit = function
          LLLitBool bool -> L.const_int i1_t (if bool then 1 else 0)
        | LLLitInt int -> L.const_int i32_t int
        | LLLitFloat float ->L.const_float float_t float
        | _ -> L.const_int i32_t 0
      in

      let build_terminator = function
          LLBlockReturn reg -> (match reg with
                                  LLRegLabel (a,b) -> L.build_ret (L.const_int i32_t 0) builder
                                | LLRegLit (typ,lit) -> L.build_ret (llvalue_of_lit lit) builder)
        | LLBlockBr (a,b,c) -> L.const_int i32_t 0
        | LLBlockJmp a -> L.const_int i32_t 0 in

      let build_stmt map stmt=
        let stmt_llvalue = (match stmt with
                              LLBuildPrintCall reg
                              -> L.build_call printf_func
                                   [| int_format_str; ( match reg with
                                                          LLRegLabel (regtype, regname) -> L.const_int i32_t 0
                                                        | LLRegLit (typ,lit) -> ( match lit with
                                                                              LLLitInt intval -> L.const_int i32_t intval
                                                                            | _ ->  L.const_int i32_t 0
                                                                          )
                                                      ) |] "printwhatever" builder
                            | LLBuildTerm terminator -> build_terminator terminator
                            | _ -> L.const_int i32_t 0
                           ) in
        StringMap.add ("stmt"^func.llfname) stmt_llvalue map
      in
      ignore(List.fold_left build_stmt StringMap.empty func.llfbody); (*build the entry block*)

      (* now let's build each block *)

      () (* end of build_funcs *)
    in
    List.iter build_func funcs in

  let _ = Llvm_analysis.assert_valid_module the_module in
  the_module;;
