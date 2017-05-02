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

(* represent typed variables with same name *)
(*TODO: How are we handling nested name declarations of different types?
ideally we should disallow this *) 
type var = {
	id : string; 
	var_type : Ast.typ;
}

type struct_type = {
  id : string;
  fields : (string * Ast.typ) list;
}


type trans_env = {
		(*  a stack of variables mapped to a name *)
    scope : var list VarMap.t;

    (* list of structs defined in the block *)
    structs : struct_type VarMap.t;

    (* list of already defined kernels/generators *)
    fn_map : fn_decl VarMap.t;

    (*TODO: externs : extern_decl VarMap.t; *)
		(* return type of block 
    ret_type : Sast.styp;*) 
}


let rec get_styp = function
 | Int -> SInt
 | Float -> SFloat
 | String -> SString
 | Bool -> SBool
 | Struct(l) -> SStruct(l)
 | Array(t, n) -> SArray(get_styp t, n)
 | Vector(l) -> SArray(SFloat, Some(l))
 | Ptr -> SPtr
 | Void -> SVoid

let rec get_typ = function
 | SInt -> Int
 | SFloat -> Float
 | SString -> String
 | SBool -> Bool
 | SStruct(l) -> Struct(l)
 | SArray(t, n) -> Array(get_typ t, n)
 | SPtr -> Ptr
 | SVoid -> Void
 
let get_bind_typ = function
 | Bind(_, t, _) -> t

let get_sfield_name = function
 | StructField(id, _ ) -> id

(* check expression 
tr_env: current translation environment
expr: expression to be checked 

returns the type of the expression *) 

(*TODO: Return type of literal *) 
let rec  type_of_lit tr_env = function
   | LitInt(l) -> Int
   | LitFloat(l) -> Float
   | LitStr(l) -> String
   | LitBool(l) -> Bool
   | LitStruct(id, l) -> 
    if VarMap.mem id tr_env.structs then
        let s = VarMap.find id tr_env.structs in
        if List.length s.fields != List.length l
        then let err_msg = "Wrong number of fields in struct "
                ^ id ^ ". Expected: " ^ string_of_int (List.length s.fields) ^ 
                " Got: " ^ string_of_int (List.length l) in raise(Failure err_msg)
        else    let match_lits struct_field = 
                   let match_lit b lit strct = 
                           if b then
                                   if get_sfield_name lit = (fst struct_field) then
                                           let err_msg = "Struct field " ^ (fst
                                           struct_field) ^ " takes place more " ^
                                           "than once in struct literal." in 
                                           raise (Failure err_msg)
                                   else b
                           else 
                             if get_sfield_name lit = (fst struct_field) then
                               if get_sfield_typ tr_env lit = (snd struct_field) then
                                    true
                                 else raise (Failure "Type mismatch")
                               else
                                    false
                   in 
                   List.fold_left (fun b x -> match_lit b x struct_field) false l
                in 
                let match_fields b sfield  = b && match_lits sfield in
                if (List.fold_left match_fields true s.fields) then Struct(id)
                else let err_msg = "Struct literal doesn't match struct defined
                as" ^ id in raise(Failure err_msg)
        else let err_msg = "Struct " ^ id ^ "is not defined"
        in raise (Failure err_msg)        
   | LitVector(l) -> Vector (List.length l)
   | LitArray(l) ->
			let rec array_check arr typ = 
        if (arr = []) then Array(typ, Some (List.length l)) else
        let nxt_typ = check_expr tr_env (List.hd arr) in
        if nxt_typ != typ then raise (Failure ("Array types not consistent."))
        else array_check (List.tl arr) (check_expr tr_env (List.hd arr)) in
      array_check (List.tl l) (check_expr tr_env (List.hd l)) 
   | LitKn(l) -> let x = l.lret_expr in match x with
      | Some x -> check_expr tr_env x
      | None -> Void

and check_expr tr_env expr =
	match expr with
	 | Lit(a) -> type_of_lit tr_env a
   | Id(var) -> 
      if VarMap.mem var tr_env.scope
			then let x = List.hd (VarMap.find var tr_env.scope) in x.var_type
			else raise (Failure ("Variable " ^ var ^ "has not been declared"))
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
       | Filter | Map as fm -> let t = (match t1 with 
            | Array(v, i) -> v 
            | _ -> raise (Failure "Left hand needs to be [] for map/filter"))
       in
            if (match e2 with
            | Binop(kn, _, _) -> (match kn with
              | Id(n) ->
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
             | Filter -> if t2 = Bool then Array(t, None) else
                     raise (Failure "Filter kernel needs to return Bool")
             | Map -> if t = t2 then Array(t, None) else 
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
   | Assign(e1, e2) -> let t1 = check_expr tr_env e1 in 
          if t1 = check_expr tr_env e2 then t1 else 
		    	raise (Failure "Assignment types don't match. shux can't autocast types.")  
   | Call(str, elist) -> (match(str) with
       | Some s -> if VarMap.mem s tr_env.fn_map then
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
          else let err_msg = "Kernel " ^ s ^ "is not defined in call." in 
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
                                | Id(l) -> l
                                | _ -> raise (Failure "Access needs to be to an
                                                      ID")) 
          in
          let t1 = check_expr tr_env id in
          (match t1 with
             | Struct(id) -> if VarMap.mem id tr_env.structs
                then 
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

and get_sfield_typ tr_env = function
 | StructField(_, expr) -> check_expr tr_env expr


(* TODO: take i:n a list of globals and create a trans_env *)
 
let create_new_env decls = decls
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
                      let v = { id = s; var_type = typ } in
                      let vlist = if VarMap.mem s tr_env.scope then
                              v :: VarMap.find s tr_env.scope else [v] in 
                      check_global_inner { scope = VarMap.add s vlist tr_env.scope; 
                        structs = tr_env.structs;
                        fn_map = tr_env.fn_map } tl
              else let err_msg = "Type mismatch in let decl: " ^ _string_of_typ typ ^ 
                                 " " ^ s ^ " is assigned to " ^ _string_of_typ t2 
                                 in raise(Failure  err_msg))
           | StructDef(s) -> if VarMap.mem s.sname tr_env.structs 
                             then let err_msg = "Struct with name " ^ s.sname ^ 
                             " defined more than once." in raise(Failure err_msg)
                             else 
                 let map_fields = function
                   | Bind(m, t, id) -> (id,t)
                 in let nfields = List.map map_fields s.fields in 
                 let st = {id = s.sname; fields = nfields} in
                 check_global_inner { scope = tr_env.scope; 
                                      structs = VarMap.add s.sname st tr_env.structs;
                                      fn_map = tr_env.fn_map } tl
           | ExternDecl(e) -> 
                 let f = { fname = e.xalias; fn_typ = Kn;
                           ret_typ = e.xret_typ; formals=e.xformals;      
                           body = []; ret_expr = None } in
                 let ntr_env = 
                       { scope = tr_env.scope; structs = tr_env.structs;
                         fn_map = VarMap.add f.fname f tr_env.fn_map } in 
                 check_global_inner ntr_env tl)
                                                    
   in 
   let env_default = { scope = VarMap.empty; structs = VarMap.empty; 
                       fn_map = VarMap.empty; } in
   check_global_inner env_default g

(* main type checking goes on here *) 
let check_functions functions run_env = run_env

(* entry point *) 
let check (ns, globals, functions) = 
	let flat_ns = flatten_ns ns in
	let global_env = check_globals (globals @ (fst flat_ns)) in
	let start_env = create_new_env global_env in 
  check_functions (functions @ (snd flat_ns)) start_env;
	([], globals @ fst flat_ns, functions @ snd flat_ns)


(*				(* Checking functions *)
				if not (List.exists (fun fd -> (fd.fname = "main" && (Astprint.string_of_typ fd.ret_typ) = "int")) functions)
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions)

*)
