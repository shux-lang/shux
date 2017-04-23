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

type trans_env = {
		(*  a stack of variables mapped to a name *)
		scope: var list VarMap.t;
			
		
		(* return type of block *)
		ret_type : Sast.styp;
}

(* check expression 
tr_env: current translation environment
expr: expression to be checked 

returns the type of the expression *) 

(*TODO: Return type of literal *) 
let type_of_lit lit = lit

let check_expr tr_env expr =
	match expr with
	 | Lit(a) -> type_of_lit a
   | Id(var) -> 
      if VarMap.mem var tr_env.scope 
			then let x = VarMap.find var tr_env.scope in x.var_type
			else raise Failure("Variable " ^ var ^ "has not been declared")
   | Binop(e1, op, e2) -> (*TODO : *) 
   | Assign(e1, e2) -> (*TODO: *) 
   | Call(str, elist) -> (*TODO *)
   | Uniop(unop, e) -> (*TODO: *) 
   | LookbackDefault(e1, e2) -> (*TODO *) 
   | Cond(e1, e2, e3) -> (*TODO: )  

(* TODO: take in a list of globals and create a trans_env *) 
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

(*TODO: *) 
let check_globals a = a

(* main type checking goes on here *) 
let check_functions functions run_env = run_env

(* entry point *) 
let check (ns, globals, functions) = 
	let flat_ns = flatten_ns ns in
	let global_env = check_globals (globals @ (fst flat_ns)) in
	let start_env = create_new_env global_env in 
	(* check_functions (functions @ (snd flat_ns)) global_env *)
	([], globals @ fst flat_ns, functions @ snd flat_ns)


(*				(* Checking functions *)
				if not (List.exists (fun fd -> (fd.fname = "main" && (Astprint.string_of_typ fd.ret_typ) = "int")) functions)
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions)

*)
