(* let's do some very basic checking here *) 

open Ast
open Astprint

module GlobalMap = Map.Make(String)

let check (ns, globals, functions) = 
				(* Checking functions *)
				if not (List.exists (fun fd -> (fd.fname = "main" && (Astprint.string_of_typ fd.ret_typ) = "int")) functions)
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions) 
