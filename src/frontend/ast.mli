(* LVS Copied from Note-Hashtag *)

open Core.Std

type type_name = string
type t =
  | Unit
  | Int
  | Float
  | String
  | Bool
  | Type of type_name
  | Array of t

type binary_operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte
  | And | Or | Zip
  | Concat | Chord | Octave

type unary_operator =
  | Not
  | Neg
  | Sharp
  | Flat

type var_reference = string list

type mutability = Mutable | Immutable

type expr =
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string
  | VarRef of var_reference
  | FunApply of string * expr list
  | ArrIdx of var_reference * expr
  | Arr of expr list * t option
  | ArrMusic of expr list 
  | Block of expr list
  | Conditional of expr * expr * expr
  | For of string * expr * expr
  | Throw of expr
  | Assign of var_reference * expr * mutability
  | StructInit of string * expr list

type fundef =
  | FunDef of string * string list * expr

type externfun =
  (* Header file name, namespace, C++ function name, NH function name, list of param types, return type *)
  | ExternFunDecl of string * string * string * string * t list * t

type typedef =
  | TypeDef of string * expr list

type program = string list * fundef list * externfun list * expr list * typedef list

let rec string_of_type t =
  match t with
  | Unit -> "unit"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Type(name) -> name
  | Array(t) -> string_of_type t ^ "{}"

let string_of_type_op t =
  match t with
  |Some t -> string_of_type t
  |None -> "None"

let string_of_op o =
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Lte -> "<="
  | And -> "&&"
  | Or  -> "||"
  | Concat -> "."
  | Chord -> ","
  | Zip -> ":"
  | Octave -> "@"

let string_of_unop o = match o with | Not -> "!" | Neg -> "-" | Sharp -> "#"
                    | Flat -> "b"

let rec string_of_expr e =
  match e with
  | Block l -> String.concat ~sep:" " [ "["; string_of_exp_list l; "]" ]
  | Conditional(x, y, z) ->
      String.concat ~sep:" " [ "IF"; string_of_expr x; "THEN"; string_of_expr y; "ELSE"; string_of_expr z ]
  | For(x, y, z) -> String.concat ~sep:" " [ "FOR"; x; "IN "; string_of_expr y; "DO"; string_of_expr z ]
  | Binop(x, op, y) -> String.concat ~sep:" " [ "("; string_of_expr x; string_of_op op; string_of_expr y; ")" ]
  | Uniop(op, x) -> String.concat ~sep:" " [ "("; string_of_unop op; string_of_expr x; ")" ]
  | LitBool(x) -> Bool.to_string x
  | LitInt(x) -> Int.to_string x
  | LitFloat(x) -> Float.to_string x
  | LitStr(x) -> x
  | VarRef(x) -> String.concat ~sep:"$" x
  | Assign(name, expr, mutability) ->
      sprintf "( %s %s = %s )"
        (if mutability = Immutable then "let" else "var") (string_of_expr (VarRef(name))) (string_of_expr expr)
  | StructInit(x, y) -> String.concat ~sep:" " [ x; string_of_exp_list y ]
  | FunApply(x, y) -> String.concat ~sep:" " [ x; "("; string_of_exp_list y; ")" ]
  | ArrIdx (x, y) -> String.concat ~sep:" " [ string_of_expr (VarRef(x)); ".("; string_of_expr y; ")" ]
  | Arr(x, t) -> String.concat ~sep:" " [ "{"; string_of_exp_list x; "}, "; string_of_type_op t ]
  | ArrMusic(x) -> String.concat ~sep:" " [ "["; string_of_exp_list x; "]"]
  | Throw(x) -> String.concat ~sep:" " ["Throw"; string_of_expr x]
and string_of_exp_list l =
  match l with
  | [] -> ""
  | [ s ] -> string_of_expr s
  | s :: rest -> String.concat ~sep:" " [ string_of_expr s; ","; string_of_exp_list rest ]

let string_of_fdef fdef =
  let FunDef(name, args, body) = fdef in
  String.concat ~sep:" " ([ name ] @ args @ [ string_of_expr body ])

let string_of_extern extern =
  let ExternFunDecl(hpp, ns, cpp_name, nh_name, param_types, ret_type) = extern in
    let cpp_path = String.concat ~sep:"::" [ hpp; ns; cpp_name ] in
    let type_strs = List.map param_types ~f:string_of_type in
    String.concat ~sep:" " ([ "extern"; cpp_path] @ type_strs @ ["->"; string_of_type ret_type; "as"; nh_name ])

let rec string_of_incl_list p =
  match p with
  | [] -> "[]"
  | s :: rest -> String.concat ~sep:" " [ s; string_of_incl_list rest ]

let rec string_of_typedefs typedefs =
  match typedefs with
  | [] -> ""
  | TypeDef(name, exprs) :: rest -> name ^ string_of_exp_list exprs ^ "\n" ^ string_of_typedefs rest

let string_of_prog_struc p =
  match p with
  | (incls, fdefs, externs, exprs, typedefs) ->
      String.concat ~sep:"\n"
        [ "INCLUDES: " ^ string_of_incl_list incls;
          "TYPEDEFS: " ^ string_of_typedefs typedefs;
          "FDEF: " ^ String.concat ~sep:"\n" (List.map fdefs ~f:string_of_fdef);
          "EXTFUN: " ^ String.concat ~sep:"\n" (List.map externs ~f:string_of_extern);
          "EXPR: " ^ string_of_exp_list exprs;
        ]