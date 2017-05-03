open Ast

type id_err = string
exception BinopTypErr of ((* left *) typ * bin_op * (* right *) typ)
exception UnopTypErr of (un_op * typ)
exception AccessErr of ((* struct *) id_err * (* member *) typ *  (* member *) id_err)
exception CondTypErr of ((* if *) typ * (* then *) typ * (* else *) typ)
exception UndeclaredId of id_err
exception NameConflict of id_err
exception GnCallErr of id_err 
exception FnCallTypErr of ((* fn *) id_err * (* return *) typ * (* expected *) typ)
exception FnArgArr of ((* fn *) id_err * (* actual *) typ * (* formal *) typ)
exception ForbiddenBindTypErr of (id_err * typ)
