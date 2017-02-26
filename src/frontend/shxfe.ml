(* shux frontend entry point -- very basic *)
open Scanner
open Parser
open Log
let _ = 
	let lexbuf = 
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else Log.error "Not enough arguments."; exit 4
		in
	let lexbuf = Lexin.from_channel lexbuf in
	let ast = Parser.program Scanner.token lexbuf
	in print_endline ast
