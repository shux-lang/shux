open Ast

(* Pretty printing *)
let nop = fun x -> x

(* for option types *)
let string_of_opt f x =
	match x with
	| None -> ""  
	| Some y -> f y

let rec _string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool" 
  | Struct t -> "struct " ^ t 
  | Array t -> (_string_of_typ t) ^ "[]"
  | Vector t -> "vector<" ^ string_of_int(t) ^ ">"

let string_of_typ x = string_of_opt _string_of_typ x

let string_of_fn_typ = function
	| Kn -> "kn"
	| Gn -> "gn"

let	string_of_op = function
	| Add -> "+"
	| Sub -> "-"
	| Mul -> "*"
	| Div -> "/"
	| Mod -> "%"
	| Exp	-> "^"
	| Eq -> "=="
	| Lt -> "<"
	| Gt -> ">"
	| Neq -> "!="
	| Leq -> "<="
	| Geq -> ">="
	| LogAnd -> "&&"
	| LogOr -> "||"
	| Filter -> "filter"
	| Map -> "map"
	| For -> "for"
	| Do -> "do"
	|	Asn -> "="
	|	AddAsn -> "+="
	|	SubAsn -> "-="
	|	MulAsn -> "*="
	|	DivAsn -> "/="
	|	ModAsn -> "%="
	|	ExpAsn -> "^="
	|	Index -> "index"
	|	Lookback -> "lookback"
	|	Access -> "access"

let string_of_unop = function
	| LogNot -> "!"
	| Neg -> "-"
	| Pos -> "+"

let string_of_mut = function
	| Immutable -> ""
	| Mutable -> "var"


let rec string_of_list f l = String.concat ", " (List.map f (List.map string_of_expr l))
  
and string_of_lit = function
	| LitInt(l) -> string_of_int l
	| LitFloat(l) -> string_of_float l
	| LitBool(l) -> string_of_bool l
	| LitStr(l) -> l
  | LitKn(l) -> ""
  | LitVector(l) -> "<" ^ string_of_list nop l ^ ">"
  | LitArray(l) -> "[" ^ string_of_list nop l ^ "]"
  | LitStruct(l) -> ""

and string_of_expr = function
 | Lit(l) -> string_of_lit l
 | Id(s) -> s
 | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
 | Call(s, el) -> (string_of_opt nop s) ^ "(" ^ string_of_list nop el ^ ")"
 | Uniop(u, e) -> string_of_unop u ^ string_of_expr e
 | Cond(e1, e2, e3) -> string_of_expr e1 ^ "?" ^ string_of_expr e2 ^ ":" ^ string_of_expr e3 (*ternary op *)

let string_of_bind bind =
	match bind with Bind(mut, typ, id) ->
	string_of_mut mut ^ _string_of_typ typ ^ id

let string_of_vdecl bind expr = 
	string_of_bind bind ^ string_of_expr expr

let string_of_stmt = function 
  | VDecl (bind, expr) -> string_of_opt (string_of_vdecl bind) expr ^ ";\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"

(* TODO: Lambda *) 
(* TODO: Struct field *)


let string_of_fdecl fdecl =
	string_of_fn_typ fdecl.fn_typ ^ " " ^ fdecl.fname ^ " " ^
	"(" ^ String.concat ", " (List.map (fun b -> _string_of_typ (match b with Bind(mut, typ, id) -> typ)) fdecl.formals) ^ ")  " ^ 
	string_of_typ fdecl.ret_typ ^ "\n{\n" ^ 
	String.concat "" (List.map string_of_stmt fdecl.body) ^ 
	string_of_opt string_of_expr fdecl.ret_expr ^
	"\n}\n"

let string_of_let = function
	| LetDecl(bind, expr) -> string_of_bind bind ^ " " ^ string_of_expr expr
	| StructDef(s) -> "" (*TODO*) 
	| ExternDecl(s) -> s.exfname ^ " " ^ string_of_typ s.exret_typ ^ " (" ^
		String.concat ", " (List.map (fun b-> match b with Bind(mut, typ, id) -> id) s.exformals) ^ ") "

let string_of_program (ns_list, let_list, fn_list) =
	(*TODO: ns_list *) 
	String.concat "" (List.map string_of_let let_list) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl fn_list) 
