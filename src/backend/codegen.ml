module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* Convention: functions and types declared in codegen.ml are named with prefix g_ *)

let translate (namespaces, globals, functions) =
  let shux_llcontext = L.global_context () in (* we only need a global data container *)
  let the_module = L.create_module shux_llcontext "shux" (* Container *)
  
  (* define lltypes *)
  and g_i32_t  = L.i32_type  shux_llcontext
  and g_i8_t   = L.i8_type   shux_llcontext
  and g_i1_t = L.i1_type shux_llcontext
  and g_float_t = L.double_type shux_llcontext
  and g_void_t = L.void_type shux_llcontext in 
  let g_str_t = L.pointer_type g_i8_t in

  (* TODO: placeholders: struct, array and vector *)
  let ltype_of_typ = function
    | A.Int -> g_i32_t
    | A.Float -> g_float_t
    | A.String -> g_i32_t
    | A.Bool -> g_i1_t
    | A.Struct struct_name -> g_i32_t
    | A.Array  element_type -> g_i32_t
    | A.Vector count -> g_i32_t in

  let ltype_of_typ_opt = function 
    | None -> g_void_t
    | Some y -> ltype_of_typ y in 

  (* TODO: ignore var versus val for now, because this should be done in the semantic checking *)
  let ltype_of_binding = function
    | A.Bind (mut_var, typ_var, name_var) -> (ltype_of_typ typ_var) in


  (* Declare printf(), which later we will change from built-in to linked extern function *)

  (* printf_t is an llvm function type that takes in an array consisting a single element g_str_t, returning g_i32_t *)
  let printf_t = L.var_arg_function_type g_i32_t [| g_str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

   (* function_decls does two things: *)
  (* 1 - define a function, along with its signature, in the module *)
  (* 2 - returns a StringMap mapping from its function name to its llvm function type *)
  let function_decls = 
    (* function_decl takes in a StringMap, add an entry mapping from function_name to its llvm function type *)
    let function_decl map fdecl =
      let function_name = fdecl.A.fname
      (* and function_type = fdecl.A.fn_typ *)
      and formal_types = 
        Array.of_list (List.map ltype_of_binding fdecl.A.formals) in 
      let function_sign_type = (* the function signature is defined by its function formal and its return type *)
        L.function_type (ltype_of_typ_opt fdecl.A.ret_typ) formal_types in 
      StringMap.add function_name (L.define_function function_name function_sign_type the_module, fdecl) map
    in
    List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl = 
    (* Prep work *)
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder_global = L.builder_at_end shux_llcontext (L.entry_block the_function) in 

    (* declare formatters as global static variables in the llvm program *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder_global in 
    let float_format_str = L.build_global_stringptr "%.5f\n" "fmt" builder_global in (* TODO make the precision configurable*)
    let format_str = L.build_global_stringptr "%s\n" "fmt" builder_global in 


    (* Construct local variables, TODO later 
      this is hard because we have mixed decl of bindings and exprs *)

    (* Construct expr builders, only implementing Call now, Lit using placeholders *)
    let rec construct_expr builder = function
      | A.Lit i ->  
        (match i with 
          | A.LitInt j -> L.const_int g_i32_t j
          | A.LitFloat j -> L.const_float g_float_t j
          | A.LitBool b -> L.const_int g_i1_t (if b then 1 else 0)
          | A.LitKn l-> L.const_int g_i32_t 0
          | A.LitVector elist -> L.const_int g_i32_t 0
          | A.LitArray elist -> L.const_int g_i32_t 0
          | A.LitStruct sflist -> L.const_int g_i32_t 0
          | A.LitStr str -> L.build_global_stringptr str "mystring" builder
        )
      | A.Id str -> L.const_int g_i32_t 0
      | A.Binop (expr, binop, expr2) -> L.const_int g_i32_t 0
      | A.Assign (expr, expr2) -> L.const_int g_i32_t 0 (* Create a local var if not string *)
      | A.Call (func, [expr]) -> 
        (match func with
          | None -> L.const_int g_i32_t 0
          | Some y -> 
            (match y with 
              | "print" -> L.build_call printf_func [| format_str; (construct_expr builder expr) |] "printf" builder
              | "print_int" -> L.build_call printf_func [| int_format_str; (construct_expr builder expr) |] "printf" builder
              | "print_float" -> L.build_call printf_func [| float_format_str; (construct_expr builder expr) |] "printf" builder
              | _ -> L.const_int g_i32_t 0
            )
        )
      | A.Call (_, l) -> L.const_int g_i32_t 0 (* oh my gosh horrible *)
      | A.Uniop (unop, expr) -> L.const_int g_i32_t 0
      | A.Cond (expr_if, expr_val, expr_else) -> L.const_int g_i32_t 0
    in
    
    (* define the terminal adder for each basic block *)
    let add_terminal builder f = 
      match L.block_terminator (L.insertion_block builder) with
        | Some _ -> ()
        | None -> ignore (f builder) in

    (* Construct stmt builders *)
    let rec construct_stmt builder = function 
    | A.VDecl (binding, expr_opt) -> builder; (* TODO: implement variable bindings here *)
    | A.Expr e -> ignore (construct_expr builder e); builder in

    (* Build the code for each statement in the function 
    let builder = construct_stmt builder (A.Block fdecl.A.body) in
    *)
    let builder_global = List.fold_left construct_stmt builder_global fdecl.A.body in 
    
    (* Add a dummy return in builder here, TODO: make it take the value from fdecl.A.ret_expr*)
    
    add_terminal builder_global (L.build_ret (L.const_int (ltype_of_typ A.Int)0))
  in
  (* End of build_function_body *)

  List.iter build_function_body functions;
  Llvm_analysis.assert_valid_module the_module; (* check correctness *) 
  the_module
