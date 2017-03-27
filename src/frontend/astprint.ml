open Ast

(* Pretty printing *)
let nop x = x

(* option helpers *)
let is_some = function
  | Some _ -> true
  | None -> false

let string_of_opt_default d f = function
  | Some s -> f s
  | None -> d

let string_of_opt f s = string_of_opt_default "" f s

let rec _string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool" 
  | Struct t -> "struct " ^ t 
  | Array t -> _string_of_typ t ^ "[]"
  | Vector t -> "vector<" ^ string_of_int t ^ ">"

let string_of_typ x = string_of_opt _string_of_typ x

let string_of_fn_typ = function
  | Kn -> "kn"
  | Gn -> "gn"

type op_typ = Infix | Prefix | PostfixPair

let _fix = function
  | Add | Sub | Mul | Div | Mod | Exp 
  | Eq | Lt | Gt | Neq | Leq | Geq | LogAnd | LogOr
  | Asn | AddAsn | SubAsn | MulAsn | DivAsn | ModAsn | ExpAsn
  | Filter | Map | Lookback | Access -> Infix
  | For | Do -> Prefix
  | Index -> PostfixPair

let string_of_binop = function
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " %"
  | Exp  -> " ^ "
  | Eq -> " == "
  | Lt -> " < "
  | Gt -> " >"
  | Neq -> " != "
  | Leq -> " <= "
  | Geq -> " >= "
  | LogAnd -> " && "
  | LogOr -> " || "
  | Filter -> " :: "
  | Map -> " @ "
  | For -> "for "
  | Do -> "do "
  |  Asn -> " = "
  |  AddAsn -> " += "
  |  SubAsn -> " -= "
  |  MulAsn -> " *= "
  |  DivAsn -> " /= "
  |  ModAsn -> " %= "
  |  ExpAsn -> " ^= "
  |  Lookback -> ".."
  |  Access -> "."
  | _ -> "" (* should raise error *)

let string_of_binop_match = function
  | Index -> ("[", "]")
  | _ -> ("", "") (* should raise error *)

let string_of_binop_expr f l o r =
  match _fix o with
  | Infix -> f l ^ string_of_binop o ^ f r
  | Prefix -> string_of_binop o ^ f l ^ f r 
  | PostfixPair -> match string_of_binop_match o with (o, c) -> f l ^ o ^ f r ^ c

let string_of_unop = function
  | LogNot -> "!"
  | Neg -> "-"
  | Pos -> "+"

let string_of_uniop_expr f o e = string_of_unop o ^ f e

let string_of_cond_expr f i t e =
  f i ^ " ? " ^ f t ^ " : " ^ f e

let string_of_mut = function
  | Immutable -> ""
  | Mutable -> "var "

let string_of_bind = function
  | Bind(mut, typ, id) -> string_of_mut mut ^ _string_of_typ typ ^ " " ^ id

let string_of_list f l o s c e =
  match (List.map f l) with
  | [] -> if e then o ^ c else ""
  | l -> o ^ String.concat s l ^ c

let rec string_of_struct_field = function
  | StructField(n, e) -> "\t." ^ n ^ " = " ^ string_of_expr e

and string_of_lambda l = 
  string_of_list string_of_bind l.lformals "(" ", " ")" false ^
  string_of_list string_of_stmt l.lbody "{\n" "" "" true ^
  string_of_opt string_of_expr l.lret_expr

and string_of_lit = function
  | LitInt(l) -> string_of_int l
  | LitFloat(l) -> string_of_float l
  | LitBool(l) -> string_of_bool l
  | LitStr(l) -> "\"" ^ l ^ "\""
  | LitKn(l) -> string_of_lambda l
  | LitVector(l) -> string_of_list string_of_expr l "<" ", " ">" true
  | LitArray(l) -> string_of_list string_of_expr l "[" ", " "]" true
  | LitStruct(l) -> string_of_list string_of_struct_field l "{" ";\n" "}" true

and string_of_expr = function
 | Lit l -> string_of_lit l
 | Id s -> s
 | Uniop(o, e) -> string_of_uniop_expr string_of_expr o e
 | Binop(e1, o, e2) -> string_of_binop_expr string_of_expr e1 o e2
 | Call(s, el) -> string_of_opt_default "_" nop s ^ 
                  string_of_list string_of_expr el "(" ", " ")" (is_some s)
 | Cond(i, t, e) -> string_of_cond_expr string_of_expr i t e

and string_of_vdecl bind expr = 
  string_of_bind bind ^ " " ^ string_of_expr expr

and string_of_stmt = function 
  | VDecl (bind, expr) -> string_of_opt (string_of_vdecl bind) expr ^ ";\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"

let string_of_fdecl fdecl =
  string_of_fn_typ fdecl.fn_typ ^ " " ^ fdecl.fname ^
  "(" ^ String.concat ", " (List.map string_of_bind fdecl.formals) ^ ") " ^
  string_of_typ fdecl.ret_typ ^ "\n{\n" ^ 
  String.concat "\n" (List.map string_of_stmt fdecl.body) ^ 
  string_of_opt string_of_expr fdecl.ret_expr ^
  "\n}\n"

let string_of_struct_def s =
  "struct " ^ s.sname ^ string_of_list string_of_bind s.fields "{\n\t" ";\n\t" "}" true

let string_of_let = function
  | LetDecl(bind, expr) -> string_of_bind bind ^ " " ^ string_of_expr expr ^ ";"
  | StructDef(s) -> string_of_struct_def s 
  | ExternDecl(s) -> "extern " ^ s.exfname ^ "(" ^
                      String.concat ", " (List.map string_of_bind s.exformals) ^ ") " ^ 
                      string_of_typ s.exret_typ ^ ";"

let string_of_program (ns_list, let_list, fn_list) =
  (*TODO: ns_list *) 
  String.concat "\n" (List.map string_of_let let_list) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl fn_list) 
