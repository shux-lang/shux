open Ast
open Sast
open Astprint

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
type var = {
	id : string; 
	obj_type : Ast.typ;
}

type trans_env = {
		(*  a stack of objects mapped to a name *)
		scope: var list VarMap.t;
	
		
		(* return type of block *)
		ret_type : Sast.typ;
}

(* TODO: take in a list of globals and create a trans_env *) 
let create_new_env decls = decls

(*TODO: Flatten namespaces
	- create new Ast.program of flattened ns and return *)
let flatten_namespaces ns = ns 

(*TODO: *) 
let check_globals a = a

(* main type checking goes on here *) 
let check_functions run_env = run_env

(* entry point *) 
let check (ns, globals, functions) = 
	let flat_ns = flatten_namespaces ns in
	let global_env = check_globals (globals @ (snd flat_ns)) in
	let start_env = create_new_env global_env in 
	check_functions global_env


(*				(* Checking functions *)
				if not (List.exists (fun fd -> (fd.fname = "main" && (Astprint.string_of_typ fd.ret_typ) = "int")) functions)
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions)

*)
