(* shux frontend entry point -- very basic *)
open Scanner
open Parser
open Semant
open Llvm
open Codegen
open Printf 
open Astprint

type action = Ast | LLVM

let _ = 
	let (cin, action) = 
    if Array.length Sys.argv = 1 then
       raise (Failure "Argument spec: ./shuxc [-a / -l] [program name]")
       else if Array.length Sys.argv = 3 then
			let a = match Sys.argv.(1) with
				| ("-a") -> Ast
				| ("-l") -> LLVM 
				| _ -> LLVM in
			let c = open_in Sys.argv.(2) in
			(c, a)
		else (open_in Sys.argv.(1), LLVM) in
	let lexbuf = Lexing.from_channel cin in
	let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
	match action with
		| Ast -> print_string (Astprint.string_of_program sast)
		| LLVM -> 
				let code = Codegen.translate sast in
    		print_string (Llvm.string_of_llmodule code)
