module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (namespaces, globals, functions) =
	let context = L.global_context () in
	let the_module = L.create_module context "shux"
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
    | A.Vector count -> i32_t
    | A.Void -> void_t in

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
            None -> A.Void (* whether void or optional, revising ast.mli, astprint.ml *)
          | Some y -> y in
      let function_sign_type = 
        L.function_type (ltype_of_typ (return_type_opt fdecl.A.ret_typ)) formal_types in 
      StringMap.add function_name (L.define_function function_name function_sign_type the_module, fdecl) map
    in
  List.fold_left function_decl StringMap.empty functions
  in



  the_module