(* shux frontend entry point -- very basic *)
open Scanner
open Parser
open Semant
open Llvm
open Codegen
open Printf 

let _ = 
	let cin  = 
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else 
                stdin;
                print_string "Not enough arguments"; 
                exit 4
		in
	let lexbuf = Lexing.from_channel cin in
	let ast = Parser.program Scanner.token lexbuf in
    let sast = Semant.check ast in 
    let code = Codegen.translate sast in
    Llvm_analysis.assert_valid_module code; (* we can do these outside *) 
    print_string (Llvm.string_of_llmodule code)
