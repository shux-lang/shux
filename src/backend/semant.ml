(* let's do some very basic checking here *) 

open Ast

module GlobalMap = Map.Make(String)

let check (ns, globals, functions) = 
				(* Checking functions *)
				if not List.exists (fun fd -> fd.fname = "main" && fd.fn_typ = Int) functions
				then raise (Failure ("no main method given")) else ();
				(ns, globals, functions) 
