module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (namespaces, globals, functions) =
	let context = L.global_context () in (* we only need a global data container *)
	let the_module = L.create_module context "shux" (* Container *)
  in the_module
  (*
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  (* and i1_t = L.i1_type context *) (* reserved for bool *)
  and void_t = L.void_type context in

  (* TODO: Other than Int all are placeholders *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> i32_t
    | A.String -> i32_t
    | A.Bool -> i32_t
    | A.Struct struct_name -> i32_t
    | A.Array  element_type -> i32_t
    | A.Vector count -> i32_t
  in
  let ltype_of_typ_opt = function 
    | None -> void_t
    | Some y -> ltype_of_typ y in 

  let global_var_count = 0 in

  (* Declare printf(), which later we will change from built-in to linked extern function *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function, including args and ret type *)
  let function_decls = 
    let function_decl map fdecl = 
      let function_name = fdecl.A.fname
      (* and function_type = fdecl.A.fn_typ *)
      and formal_types = 
        Array.of_list []  (* TODO: empty formals for now *) in 
      let function_sign_type = 
        L.function_type (ltype_of_typ_opt fdecl.A.ret_typ) formal_types in 
      StringMap.add function_name (L.define_function function_name function_sign_type the_module, fdecl) map
    in
  List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl = 
    (* Prep work *)
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder_global = L.builder_at_end context (L.entry_block the_function) in 
    
    (*
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder_global in 
    *)
    let format_str = L.build_global_stringptr "%s\n" "fmt" builder_global in 

    (* Construct local variables, TODO later 
      this is hard because we have mixed decl of bindings and exprs *)

    (* Construct expr builders, only implementing Call now, Lit using placeholders *)
    let rec construct_expr builder = function
      | A.Lit i ->  (match i with 
                      | A.LitInt j -> L.const_int i32_t j
                      | A.LitFloat j -> L.const_int i32_t 0
                      | A.LitBool b -> L.const_int i32_t 0
                      | A.LitKn l-> L.const_int i32_t 0
                      | A.LitVector elist -> L.const_int i32_t 0
                      | A.LitArray elist -> L.const_int i32_t 0
                      | A.LitStruct sflist -> L.const_int i32_t 0
                      | A.LitStr str -> ignore(global_var_count = global_var_count+1); L.build_global_stringptr str ("mystring"^(string_of_int global_var_count)) builder
                    )
      | A.Id str -> L.const_int i32_t 0
      | A.Binop (expr, binop, expr2) -> L.const_int i32_t 0
      | A.Assign (expr, expr2) -> L.const_int i32_t 0 (* Create a local var if not string *)
      | A.Call (func, [expr]) -> (match func with
                                  | None -> L.const_int i32_t 0
                                  | Some y -> (match y with 
                                                | "print" -> L.build_call printf_func [| format_str; (construct_expr builder expr) |] "printf" builder
                                                | _ -> L.const_int i32_t 0
                                              )
                                 )
      | A.Call (_, l) -> L.const_int i32_t 0 (* oh my gosh horrible *)
      | A.Uniop (unop, expr) -> L.const_int i32_t 0
      | A.Cond (expr_if, expr_val, expr_else) -> L.const_int i32_t 0
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
  *)
