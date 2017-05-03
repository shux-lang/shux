open Ast
open Sast

exception BinopTypErr of { ltyp: typ; binop: bin_op; rtyp: typ }
exception UnopTypErr of { unop: un_op; utyp: typ }
exception AccessErr of { id: string; struct_typ: typ; field: string }
exception CondTypErr of { if_typ: typ; then_typ: typ; else_typ: typ }
exception UndeclaredId of string
exception NameConflict of string
exception GnCallErr of string
exception FnCallTypErr of { fn_id: string; fn_ret_typ: typ; (* expected type *) exp_typ: typ }
exception FnArgArr of { fn_id string; fn_formal_typ: typ; fn_actual_typ: typ }
exception ForbiddenBindTypErr of { id: string; bind_typ: typ }
exception InternalErr of { why: string; ctx: string } (* flexible generic illegal internal funsies *)
