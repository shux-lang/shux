open Ast
open Astprint
open Sast

(* map variable to type 
	or stack of types if overridden *) 
module VarMap = Map.Make(struct
        type t = string
        let compare x y = Pervasives.compare x y
		end) 

(* Set of var names *) 
module VarSet = Set.Make(struct
      type t = string
      let compare x y = Pervasives.compare x y
		end)

module StringMap = Map.Make(String)

type struct_type = {
  struct_id : string;
  fields : (string * Ast.typ) list;
}

type var = {
	id : string;
  mut : Ast.mut;
	var_type : Ast.typ;
	initialized : bool;
}

type trans_env = {
		(*  a stack of variables mapped to a name *)
    scope : var list VarMap.t;

    (* list of structs defined in the block *)
    structs : struct_type VarMap.t;

    (* list of already defined kernels/generators *)
    fn_map : fn_decl VarMap.t;

		(* new variables defined within a scope
       to ensure that name conflicts dont happen
       within a scope  *) 
		new_variables : var list;
}

let flatten_ns_list ns_list = 
    let rec flatten_ns_rec flat = function
        | [] -> flat
        | hd::tl -> flatten_ns_rec (flat ^ "_" ^ hd) tl
    in flatten_ns_rec (List.hd ns_list) (List.tl ns_list)

let get_sfield_name = function
 | StructField(id, _ ) -> id

let get_bind_typ = function
   | Bind(_,t,_) -> (match t with
       | Struct(str_list) -> Struct([flatten_ns_list str_list])
       | _ -> t)
 
let get_bind_name = function
   | Bind(_,_,s) -> s

let get_bind_mut = function
   | Bind(m,_,_) -> m

let convert_ret_typ = function
   | Some x -> x
   | None -> Void

let compare_ast_typ l r = match(l,r) with
   | (Array(t1, None), Array(t2, Some i)) -> t1=t2
   | (l,r) -> l=r

(* ensure that arrays are initialized properly when declared *) 
let check_array_init = function
    | Array(t,i) -> (match i with
        | Some i -> true
        | None -> raise (Failure "Arrays need to have sizes to be initialized."))
    | _ -> true

(* ensure that a variable isnt part of new_variables when its defined *) 
let check_var_notdef var env = 
    let check_var v b new_var = 
       if b then 
          if v.id = new_var.id then false else b
       else false in
    List.fold_left (check_var var) true env.new_variables
 
(* called within the assign expression to initialize variable in env *)
let initialize_var name env = 
    let var_list = VarMap.find name env.scope
    in let var = List.hd var_list
    in let new_var = { id = var.id; mut=var.mut; var_type = var.var_type;
                   initialized = true; }
    in let new_scope = VarMap.add name (new_var :: List.tl var_list) env.scope
    in { scope = new_scope; structs = env.structs; fn_map = env.fn_map;
         new_variables = env.new_variables }

(* return new trans env with var v added *) 
let push_variable_env v env = 
    if not (check_var_notdef v env)
        then let err_msg = v.id ^ " defined more than once in scope." in 
                 raise (Failure err_msg) 
    else let new_scope = 
        if VarMap.mem v.id env.scope then
            let oldvarlist = VarMap.find v.id env.scope
            in VarMap.add v.id (v::oldvarlist) env.scope
        else 
            VarMap.add v.id [v] env.scope
    in { scope = new_scope; structs = env.structs; fn_map = env.fn_map;
         new_variables = v::env.new_variables } 

 
=======
         new_variables = env.new_variables } 
>>>>>>> b917e73fa0da2b4575ca9c50ab28bb0f45c31b9a
(* check expression 
tr_env: current translation environment
expr: expression to be checked 

returns the type of the expression *) 
let rec type_of_lit tr_env = function
   | LitInt(l) -> Int
   | LitFloat(l) -> Float
   | LitStr(l) -> String
   | LitBool(l) -> Bool
   | LitStruct(id, l) -> 
    let nid = flatten_ns_list id in 
    if VarMap.mem nid tr_env.structs then
        let s = VarMap.find nid tr_env.structs in
        if List.length s.fields != List.length l
        then let err_msg = "Wrong number of fields in struct "
                ^ nid ^ ". Expected: " ^ string_of_int (List.length s.fields) ^ 
                " Got: " ^ string_of_int (List.length l) in raise(Failure err_msg)
        else 
            let match_lits = match_sfields tr_env l in
            let match_fields b sfield = b && match_lits sfield in
                if (List.fold_left match_fields true s.fields) then 
                    Struct([nid])
                else 
                    let err_msg = "Struct literal doesn't match struct defined" ^ 
                                  "as" ^ nid in raise(Failure err_msg)
    else let err_msg = "Struct " ^ nid ^ " is not defined"
                       in raise (Failure err_msg)

   | LitVector(l) -> let vector_check v = 
                         if check_expr tr_env v = Float then true
                         else raise (Failure "Vector literals need to consist entirely of floats")
                      in ignore(List.map vector_check l); Vector(List.length l)
   | LitArray(l) ->
      let arr_length = List.length l in
			let rec array_check arr typ = 
        if (arr = []) then Array(typ, Some (arr_length)) else
        let nxt_typ = check_expr tr_env (List.hd arr) in
        if nxt_typ != typ then raise (Failure ("Array types not consistent."))
        else array_check (List.tl arr) (check_expr tr_env (List.hd arr)) in
      array_check (List.tl l) (check_expr tr_env (List.hd l)) 
   | LitKn(l) -> lambda_checker l tr_env

and check_expr tr_env expr =
	match expr with
	 | Lit(a) -> type_of_lit tr_env a
   | Id(nvar) -> let var = flatten_ns_list nvar in
      if VarMap.mem var tr_env.scope then
          let found_var = List.hd (VarMap.find var tr_env.scope)
          in if found_var.initialized 
              then found_var.var_type
          else 
              raise (Failure ("Variable " ^ var ^ " has not been initialized"))
			else 
          raise (Failure ("Variable " ^ var ^ " has not been declared"))
   | Binop(e1, op, e2) -> 
      let t1 = check_expr tr_env e1 in
      let t2 = check_expr tr_env e2 in
      (match op with
       | Add | Sub | Mul | Div -> if t1 != t2 
          then raise (Failure "Can't do binop on incompatible types.")
          else if t1 != Int && t2 != Float then 
             raise (Failure "Binop only defined over integers or scalars.")
         else t1
       | Mod -> if t1 = Int && t2 = Int then Int else 
          raise (Failure "Bad types for mod operator")
       | Exp -> if t1 = Float && t2 = Float then Float else

          raise (Failure "Bad types for exponent operator")
       | Eq | Lt | Gt | Neq | Leq | Geq -> if t1 != t2 
          then raise (Failure "Can't do binop on incompatible types.") else Bool
       | LogAnd | LogOr -> if t1 != t2 || t1 != Bool
          then raise (Failure "Logical and/or only applies to bools.") else Bool
       | Filter | Map as fm -> let (t,i) = (match t1 with 
            | Array(v, i) -> (v, i) 
            | _ -> raise (Failure "Left hand needs to be [] for map/filter"))
       in
            if (match e2 with
            | Binop(kn, _, _) -> (match kn with
              | Id(nn) -> let n = flatten_ns_list nn in
                  if VarMap.mem n tr_env.fn_map then
                      let k = VarMap.find n tr_env.fn_map in
                      if k.fn_typ = Kn 
                      then List.length k.formals = 1
                      else let err_msg = "Map/Filter function" ^ n
                       ^ " needs to be a kernel" in raise(Failure err_msg)
                  else let err_msg = "Kernel call in filter/map " ^ n ^ "doesnt
                  exist" in raise(Failure err_msg)
              | Lit(n) -> (match n with
                          | LitKn(l) ->(List.length l.lformals) = 1 && 
                                    (get_bind_typ (List.hd l.lformals)) = t
                          | _ -> raise (Failure "Filter not given a lambda"))
              | _ -> raise (Failure "Filter not given a lambda :((("))
            | Lit(n) -> (match n with
                         | LitKn(l) ->(List.length l.lformals) = 1 &&
                                   (get_bind_typ (List.hd l.lformals)) = t
                         | _ -> raise (Failure "Filter not given a lambda :("))
            | _ -> raise (Failure "Filter not given a lambda :((("))
            then (match fm with
             | Filter -> if t2 = Bool then Array(t, i) else
                     raise (Failure "Filter kernel needs to return Bool")
             | Map -> if t = t2 then Array(t, i) else 
                     raise (Failure "Map kernel return type needs to match
                                   array")
             | _ -> raise (Failure "The OCaml compiler has a strict type system."))
            else raise (Failure "Map/Filter needs kernel that takes single
            parameter matching the [] to be mapped/filtered")
       | Index -> if t2 = Int then match t1 with
          | Array(v, i) -> v
          | Vector(l) -> Float
          | _ -> raise (Failure "Indexing needs an array/vector to index into")
         else raise (Failure "Indexing needs to be by integer expression only.")
      | For -> Array(t2, None) (*For returns an array of the return type of gn *)
      | Do -> t2 (* Do simply returns the final value of the gn *))
   | Assign(e1, e2) -> 
            (* need to short circuit the initialization checker here *)
        let match_typ exp1 exp2 = 
            let t1 = (match exp1 with
                | Id(l) -> let flat_name = flatten_ns_list l
                           in if VarMap.mem flat_name tr_env.scope
                           then let var = List.hd (VarMap.find flat_name tr_env.scope)
                           in var.var_type
                           else raise ( Failure ("Variable " ^ flat_name ^ " is not defined")) 
                | _ -> check_expr tr_env exp1) and
                t2 = check_expr tr_env exp2 in
            if t1 = t2 then t1 
            else raise (Failure "Assignment types don't match. shux can't
            autocast types") in
        let get_mutability l =
                let v = List.hd (VarMap.find l tr_env.scope) in
                if v.mut = Mutable then v.var_type 
                else raise (Failure "Cannot assign to immutable type")
        in
   (match e1 with 
       | Id l -> ignore (match_typ e1 e2); get_mutability (flatten_ns_list l)
       | Assign(l1,l2) -> match_typ e1 e2
       | Access(x,y)-> match_typ e1 e2 
       | Binop(idx1, Index, _) -> (match idx1 with
           | Id(l) -> ignore (get_mutability (flatten_ns_list l))
           | _ -> raise (Failure "Semant not implemented for indexing into non-ids"));
       match_typ e1 e2
       | _ -> raise (Failure "Assign can only be done against an id or struct field")
   )              

   | Call(str, elist) -> (match(str) with
       | Some ns -> let s = flatten_ns_list ns in
                    if VarMap.mem s tr_env.fn_map then
                      let fn = VarMap.find s tr_env.fn_map and
                      tlist = List.map (check_expr tr_env) elist
                      in
                      if(fn.fn_typ = Gn) then raise (Failure "Cannot call
                      generator without an iterator") else 
                         let match_formal b fform cform = 
                             if b then get_bind_typ fform = cform
                             else b 
                         in
                         if (List.fold_left2 match_formal true fn.formals tlist)
                         then match(fn.ret_typ) with 
                             | Some x -> x
                             | None -> Void
                         else let err_msg = "Formals dont match for function" ^
                                 " call " ^ s in raise (Failure err_msg)
          else let err_msg = "Kernel " ^ s ^ " is not defined in call." in 
          raise (Failure err_msg)
       | None -> Int)
           
   | Uniop(unop, e) -> (match unop with
      | Pos | Neg -> let t = check_expr tr_env e in if t = Int || t = Float then t else
         raise (Failure "Pos/Neg unioperators only valid for ints or floats")
      | LogNot -> if check_expr tr_env e = Bool then Bool else 
         raise (Failure "Logical not only applies to booleans"))
   | LookbackDefault(e1, e2) -> 
          if (check_expr tr_env e2) == Int then check_expr tr_env e1 else
          raise (Failure "Lookback indexing needs to be an integer")
   | Cond(e1, e2, e3) -> if check_expr tr_env e1 = Bool then
        let t2 = check_expr tr_env e2 in if t2 = check_expr tr_env e3 then t2
        else raise (Failure "Ternary operator return type mismatch")
     else raise (Failure "Ternary operator conditional needs to be a boolean
     expr")
   | Lookback(str, i) -> check_expr tr_env (Id str)  
   | Access(id, str) -> let fname = (match id with
                                | Id(l) -> flatten_ns_list l (* for namespace *) 
                                | _ -> "") 
          in
         (* check structs *) 
          let t1 = check_expr tr_env id in
          (match t1 with
             | Struct(nid) -> let id = flatten_ns_list nid in 
                        if VarMap.mem id tr_env.structs then
                        let s = VarMap.find id tr_env.structs in
                        let find_field tuple field = 
                                if (fst tuple)="NONE" then
                                   if (fst field)=str 
                                      then (fname, snd field) else tuple
                                else tuple in
                                let matched = List.fold_left find_field ("NONE", Int)
                       s.fields in 
                       if (fst matched)="NONE" then
                               let err_msg = "Struct field named " ^ str ^ " is "  ^
                               "not defined." in raise (Failure err_msg) 
                        else (snd matched)
                else let err_msg = "Struct named " ^ id ^ " not defined." in 
                         raise (Failure err_msg)
             | _ -> raise (Failure "Can't access field of a type that's not a
                           struct"))

(* function checker for lambdas *)
(* returns the type of lambda *) 
(* basically a duplication of the code for check_body *) 
and lambda_checker l env = 
    let formal_var = List.hd l.lformals
    in let formal_name = get_bind_name formal_var
    in let formal_type = get_bind_typ formal_var
    in let m = Immutable 
    in let v = { id = formal_name; var_type = formal_type; mut = m; initialized = true } 
    in let formal_env = push_variable_env v env
    in let body = l.lbody
    in let ret = l.lret_expr
    in
    let check_stmt env = function
        | VDecl(b,e) -> (match e with
            | Some e -> let t1 = check_expr env e
                        and t2 = get_bind_typ b
                        and var_name = get_bind_name b
                        and m = get_bind_mut b in
            let _ = check_array_init t2 in
            if compare_ast_typ t2 t1 then
                let v = {id = var_name; var_type = t2; mut = m; initialized = true }
                        in push_variable_env v env
                else raise (Failure "Type mismatch in lambda body")
            | None  -> let t = get_bind_typ b
                       and var_name = get_bind_name b
                       and m = get_bind_mut b
                    in let _ = check_array_init t
                    in let v = { id = var_name; var_type = t; mut = m; initialized = true }
                    in push_variable_env v env)
        | Expr(e) -> let _ = check_expr env e in
            (match e with
            | Assign(e1,e2) -> (match e2 with 
                | Id(l) -> initialize_var (flatten_ns_list l) env
                | _ -> env)
            | _ -> env)
     in (match ret with
        | Some r -> check_expr (List.fold_left check_stmt formal_env body) r
        | None -> Void)


and get_sfield_typ tr_env = function
 | StructField(_, expr) -> check_expr tr_env expr

(* type checking helper for structs *)
and
match_sfields env lit struct_field = 
   let get_typ = get_sfield_typ env in
   let match_lit b lit strct = 
      if b then
         if get_sfield_name lit = (fst struct_field) then
             let err_msg = "Struct field " ^ (fst struct_field) ^ 
                 " takes place more than once in struct literal." in
                 raise (Failure err_msg)
         else b
      else
         if get_sfield_name lit = (fst struct_field) then
				    if get_typ lit = (snd struct_field) then
						    true
						 else let err_msg = "Type mismatch in struct literal: " ^ _string_of_typ (get_typ lit) 
                                 ^ " doesn't match " ^ _string_of_typ (snd struct_field) 
                                   in raise (Failure err_msg)
			   else
             false
    in List.fold_left (fun b x -> match_lit b x struct_field) false lit


let fltn_global nsname globs =
	let handle_glob nsname = function
		| LetDecl(Bind(m,t,n),e) ->		
				let newn = nsname ^ "_" ^ n in LetDecl(Bind(m,t,newn),e)
		| StructDef s -> StructDef( { sname = nsname ^ "_" ^ s.sname; 
                                  fields = s.fields} )
		| ExternDecl e -> ExternDecl( { xalias = nsname ^ "_" ^ e.xalias;
                                    xfname = e.xfname;
                                    xret_typ = e.xret_typ;
                                    xformals = e.xformals; } )

	in List.map (fun x -> handle_glob nsname x) globs

let fltn_fn nsname fndecls = 
	let rec handle_function nsname fndecl = 
		{ fname = nsname ^ "_" ^ fndecl.fname;
			fn_typ = fndecl.fn_typ;
			ret_typ = fndecl.ret_typ;
			formals = fndecl.formals;
			body = fndecl.body;
			ret_expr = fndecl.ret_expr; } in
	List.map (fun x -> handle_function nsname x) fndecls

let rec flatten_ns ns_list = 
	let rec handle_ns ns = 
		match ns.nbody with
		| ([], glob, fn) -> (fltn_global ns.nname glob, fltn_fn ns.nname fn)
		| (nestns, glob, fn) -> let flat_ns = flatten_ns nestns in
                          (fltn_global ns.nname (glob @ (fst flat_ns)),
                           fltn_fn ns.nname (fn @ (snd flat_ns)))
	in List.fold_left (fun (a,b) (an, bn) -> (a@an, b@bn)) ([], [])
	(List.map handle_ns ns_list)

let check_globals g =
   let rec check_global_inner tr_env = function
        | [] -> tr_env
        | hd::tl -> (match hd with
           | LetDecl(bind, expr) -> (match bind with
              | Bind(mut, typ, s) -> 
               let t2 = check_expr tr_env expr in
               if compare_ast_typ typ t2 then
                       let v = { id = s; var_type = typ; mut=mut; initialized = true} in
                      let vlist = if VarMap.mem s tr_env.scope then
                              v :: VarMap.find s tr_env.scope else [v] in 
                      check_global_inner { scope = VarMap.add s vlist tr_env.scope; 
                        structs = tr_env.structs;
                        fn_map = tr_env.fn_map; 
                        new_variables = [] } tl
              else let err_msg = "Type mismatch in let decl: " ^ _string_of_typ typ ^ 
                                 " " ^ s ^ " is assigned to " ^ _string_of_typ t2 
                                 in raise(Failure  err_msg))
           | StructDef(s) -> if VarMap.mem s.sname tr_env.structs 
                             then let err_msg = "Struct with name " ^ s.sname ^ 
                             " defined more than once." in raise(Failure err_msg)
                             else 
                             let check_unique m bind = 
                                 let name = get_bind_name bind in
                                 if StringMap.mem name m then
                                     let err_msg = "Field name " ^ name ^ " defined more than once in struct "
                                                   ^ s.sname in raise (Failure err_msg)
                                 else StringMap.add name name m
                             in ignore(List.fold_left check_unique StringMap.empty s.fields);
                 let map_fields = function
                   | Bind(m, t, id) -> if VarMap.mem (s.sname ^ "_" ^ id) tr_env.scope then
                                          let err_msg = "Namespace/struct field contention for struct " 
                                                        ^ s.sname ^ " and field " ^ id in raise(Failure err_msg)
                                       else
                                       (match t with
                                           | Array(typ, i) -> (match i with
                                               | Some i -> (id,t)
                                               | None -> let err_msg = "Array " ^ id ^ " initialized with no fixed " 
                                                                       ^ "size in struct " ^ id 
                                                                          in raise (Failure err_msg))
                                           | _ -> (id,t))
                 in let nfields = List.map map_fields s.fields in 
                 let st = {struct_id = s.sname; fields = nfields} in
                 check_global_inner { scope = tr_env.scope; 
                                      structs = VarMap.add s.sname st tr_env.structs;
                                      fn_map = tr_env.fn_map; 
                                      new_variables = [] } tl
           | ExternDecl(e) -> 
                 let f = { fname = e.xalias; fn_typ = Kn;
                           ret_typ = e.xret_typ; formals=e.xformals;      
                           body = []; ret_expr = None } in
                 let ntr_env = 
                       { scope = tr_env.scope; structs = tr_env.structs;
                         fn_map = VarMap.add f.fname f tr_env.fn_map;
                         new_variables = []} in 
                 check_global_inner ntr_env tl)
                                                    
   in 
   let env_default = { scope = VarMap.empty; structs = VarMap.empty; 
                       fn_map = VarMap.empty; new_variables = []} in
   check_global_inner env_default g

      
let check_body f env = 
    let check_formals formals env = 
        let check_formal env old_formals formal = 
            if List.mem formal old_formals then
                let err_msg = "Formal " ^ get_bind_name formal ^ " has been" 
                            ^ " defined more than once." in raise (Failure err_msg)
            else formal  :: old_formals 
        in List.fold_left (check_formal env) [] formals and
        place_formal env formal = 
           let formal_name = get_bind_name formal and
               formal_type = get_bind_typ formal and
               m = Immutable
           in let v = { id = formal_name; var_type = formal_type; mut = m; initialized=true }
           in push_variable_env v env
    in let formal_env = 
       List.fold_left place_formal env (check_formals f.formals env) in
    let body = f.body and
        ret = f.ret_expr 
    in
    let check_stmt env = function
        | VDecl(b,e) -> (match e with (*TODO: ensure uniqueness of bind name *) 
           | Some exp -> let t1 = check_expr env exp and
                             t2 = get_bind_typ b and
                             var_name = get_bind_name b 
                             and m = get_bind_mut b in
               (* ensure that arrays cannot be initialized without sizes *)
               let _ = check_array_init t2 in 
               if compare_ast_typ t2 t1 then
                   let v = { id = var_name; var_type = t2; mut = m; initialized = true}
                   in push_variable_env v env                  
               else let err_msg = "Type " ^ _string_of_typ t1 ^ " cannot be assigned" 
                                  ^ " to type " ^ _string_of_typ t2
                   in raise(Failure err_msg)
           | None -> let t = get_bind_typ b and (*TODO: ensure uniqueness.. *)
                         (* ensure that arrays cannot be initialized without sizes *) 
                         var_name = get_bind_name b and
                         m = get_bind_mut b 
           in let _ = check_array_init t
           in let v = { id = var_name; var_type = t; mut = m; initialized = false}
           in push_variable_env v env ) 
        | Expr(e) -> let _ = check_expr env e in
            (match e with
            | Assign(e1, e2) -> (match e2 with
                | Id(l) -> initialize_var (flatten_ns_list l) env
                | _ -> env)
            | _ -> env)
       in let ret_typ = convert_ret_typ f.ret_typ
       and tr = (match ret with
          | Some r -> check_expr (List.fold_left check_stmt formal_env body) r
          | None -> Void)
       in if (tr = ret_typ) then
                   { scope = env.scope; structs = env.structs;
                     fn_map = VarMap.add f.fname f env.fn_map;
                     new_variables = env.new_variables}
          else 
                   let err_msg  = "Function " ^ f.fname ^ " has type "
                   ^ _string_of_typ ret_typ ^ 
                   " but returns type " ^ _string_of_typ tr
                   in raise (Failure err_msg)

(* checks if main is defined *)
let check_main functions = 
        let check_if_main b f = 
                if b then b
        else if f.fname = "main" && f.fn_typ = Kn then true else b in
    List.fold_left check_if_main false functions

(* main type checking goes on here *) 
let check_functions functions run_env = 
   if check_main functions then
       let check_function tr_env f =
          if VarMap.mem f.fname tr_env.fn_map then
             let err_msg = "Function " ^ f.fname ^ " is defined more than once" in 
             raise(Failure err_msg)
          else if VarMap.mem f.fname tr_env.scope then
             let err_msg = "Function " ^ f.fname ^ " name conflicts with global"
             ^ " variable" in raise(Failure err_msg) 
          else check_body f tr_env
       in List.fold_left check_function run_env functions
    else raise (Failure "Main kn not defined") 

(* entry point *) 
let check (ns, globals, functions) = 
	let flat_ns = flatten_ns ns in
  let globs_with_ns = (fst flat_ns) @ globals in 
	let global_env = check_globals globs_with_ns in
  ignore (check_functions ((snd flat_ns) @ functions) global_env);
	([], fst flat_ns @ globals, snd flat_ns @ functions)
