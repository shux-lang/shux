module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (namespaces, globals, functions) =
	let context = L.global_context () in (* we only need a global data container *)
	let the_module = L.create_module context "shux" (* Container *)
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t = L.i1_type context 
  and void_t = L.void_type context in

  (* All placeholders *)


  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> i32_t
    | A.String -> i32_t
    | A.Bool -> i32_t
    | A.Struct struct_name -> i32_t
    | A.Array  element_type -> i32_t
    | A.Vector count -> i32_t in

  let ltype_of_typ_opt = function 
      None -> void_t
    | Some y -> ltype_of_typ y in 

  (* Declare printf(), which later we will change from built-in to linked extern function *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in


  (* Define each function, including args and ret type *)
  let function_decls = 
    let function_decl map fdecl = 
      let function_name = fdecl.A.fname
      (* and function_type = fdecl.A.fn_typ *)
      and formal_types = 
        Array.of_list []  (* empty formals for now *)
      and return_type_opt = function
            None -> void_t (* whether void or optional, revising ast.mli, astprint.ml *)
          | Some y -> y in
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
    let builder = L.builder_at_end context (L.entry_block the_function) in 
    
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 

    (* Construct local variables, TODO later 
      this is hard because we have mixed decl of bindings and exprs *)

    (* Construct expr builders, only implementing Call now, Lit using placeholders *)
    let rec construct_expr builder = function
        A.Lit i ->  (match i with 
                        A.LitInt j -> L.const_int i32_t j
                      | A.LitFloat j -> L.const_int i32_t 0
                      | A.LitBool b -> L.const_int i32_t 0
                      | A.LitKn l-> L.const_int i32_t 0
                      | A.LitVector elist -> L.const_int i32_t 0
                      | A.LitArray elist -> L.const_int i32_t 0
                      | A.LitStruct sflist -> L.const_int i32_t 0
                      | A.LitStr str -> L.const_int i32_t 0
                    )
      | A.Id str -> L.const_int i32_t 0
      | A.Binop (expr, binop, expr2) -> L.const_int i32_t 0
      | A.Call (func, [expr]) -> (match func with
                                    None -> L.const_int i32_t 0
                                  | Some y -> (match y with 
                                                "print" -> L.build_call printf_func [| int_format_str; (construct_expr builder expr) |] "printf" builder
                                                | _ -> L.const_int i32_t 0
                                              )
                                 )
      | A.Call (_, _) -> L.const_int i32_t 0 (* oh my gosh horrible *)
      | A.Uniop (unop, expr) -> L.const_int i32_t 0
      | A.Cond (expr_if, expr_val, expr_else) -> L.const_int i32_t 0
    in
    
    (* define the terminal adder for each basic block *)
    
  ()
  in
  (* End of build_function_body *)

  List.iter build_function_body functions;
  the_module