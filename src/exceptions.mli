open Ast

exception BinopTypErr of (typ * bin_op * typ)
exception UnopTypErr of (un_op * typ)
exception AccessErr of (string * typ * string)
exception CondTypErr of (typ * typ * typ)
exception UndeclaredId of string
exception NameConflict of string
exception GnCallErr of string 
exception FnCallTypErr of (string * (* actual *) typ * (* expected *) typ)
exception FnArgArr of (string * typ * typ)
exception ForbiddenBindTypErr of (string * typ )
