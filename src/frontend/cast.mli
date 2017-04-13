(* cast.ml *) 
(* Makes most sense to implement this ground-up *) 

(* I am also going to use just regular names...
   unsure how OCaml handles name type conflicts *) 
type typ = 
 | Int
 | Float
 | String
 | Bool
 | Struct of string 
 | Array of typ
 | Vector of int
 | Void

type mut = 
 | Mutable
 | Immutable

type bind = Bind of mut * typ * string

(* Struct of arrays/circbuffs *)

type kn = {
	fname		 : string;
	ret_typ	 : typ;
	formals	 : bind list;
	body		 : stmt	list;
	ret_expr : expr;
}

type vkn = {
  fname		 : string
	formals	 : bind list;
	body		 : stmt list;
}

(* Generator *) 
type buffer = typ * int

type gn_struct = buffer list

type gn = Gn of gn_struct * kn


type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Asn
  | Eq | Lt | Gt | Neq | Leq | Geq
  | LogAnd | LogOr
  | Index
  | Access


