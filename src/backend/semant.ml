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
type var = {
	id : string; 
	obj_type : Ast.typ;
}

type trans_env = {
		(*  a stack of objects mapped to a name *)
		scope: var list VarMap.t;
	
		
		(* return type of block *)
		ret_type : Sast.styp;
}

(* TODO: take in a list of globals and create a trans_env *) 
let create_new_env decls = decls

(*TODO: Flatten namespaces
	- create new Ast.program of flattened ns and return *)

let fltn_global nsname globs =
	let handle_glob nsname = function
		| LetDecl(bnd,e) -> 
				let newbnd = (fun (m,t,n) -> (m,t,nsname ^ "_" ^ n)) bnd in 
				LetDecl(bndl,e)
		| StructDef s -> StructDef( { sname = nsname ^ "_" ^ s.sname; 
																	fields = s.fields} )
		| ExternDecl e -> ExternDecl( { xalias = nsname ^ "_" ^ e.xalias;
																		xfname = e.xfname;
																		xret_typ = e.xret_typ;
																		xformals = e.xformals; } )

	in List.map (fun x -> handle_glob nsname x) globs

 (*List.fold_left *) 
let fltn_fn nsname fndecl = fndecl

let flatten_ns ns = match ns.nbody with
	| ([], glob, fn) -> (fltn_global ns.nname glob, fltn_fn ns.nname fn)
	| (nestns, glob, fn) -> let flat_ns = flatten_ns nestns in
												(fltn_global ns.nname (glob @ (fst flat_ns)),
												fltn_fn ns.nname (fn @ (snd flat_ns)))

(*TODO: *) 
let check_globals a = a

(* main type checking goes on here *) 
let check_functions functions run_env = run_env

(* entry point *) 
let check (ns, globals, functions) = 
	let flat_ns = flatten_ns ns in
	let global_env = check_globals (globals @ (fst flat_ns)) in
	let start_env = create_new_env global_env in 
	check_functions (functions @ (snd flat_ns)) global_env


(*				(* Checking functions *)
				if not (List.exists (fun fd -> (fd.fname = "main" && (Astprint.string_of_typ fd.ret_typ) = "int")) functions)
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions)

*)
