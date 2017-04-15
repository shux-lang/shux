open Sast
open Cast

let sast_to_cast let_decls f_decls =

  let walk_fns f = 
    let walk_stmts p b = 
      let walk_expr = function
        | _ -> [ExprDud]
      in let walk = function
      | _ -> [StmtDud]
      in walk b
    in let walk_gn g = 
      let ps = "gn_"
      in let pv = "gnv_"
      in let get_locals f l r =
        let prefix = function SBind(t, s) -> SBind(t, pv ^ s)
        in let collect f l r = List.map fst f @ List.map fst l @ r;
        in List.map prefix (collect f l r)
      in let rec gn_struct n = function
        _ -> DeclDud
      in let walk = function { sgname = n; sgret_typ = t; sgformals = f;
                                sglocalvals = ll; sglocalvars = lr;
                                sgbody = b; sgret_expr = r} ->
        [ gn_struct n (f @ ll);
          CFnDecl({ cfname = n; cret_typ = t; cformals = [(* the struct *)]; 
                    clocals = get_locals f ll lr; 
                    cbody = let r = walk_stmts pv b in r @ walk_stmts pv r})]
      in walk g

    in let walk_kn = function { skname = n; skret_typ = t; skformals = f;
                                sklocals = l; skbody = b; skret_expr = r} ->
      CFnDecl({ cfname = n; cret_typ = t; cformals = f; clocals = l;
                cbody = let r = walk_stmts "" b in r @ walk_stmts "" r})
    in let rec walk = function
    | [] -> []
    | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
    | SKnDecl(k)::t -> let r = walk_kn k in r :: walk t
    in walk f

  in let walk_static l = 
    let walk_expr = function (* this needs to be basically an interpreter *)
      | _ -> StmtDud
    in let walk = function
      | SLetDecl(b, e) -> CConstDecl(b, walk_expr e)
      | SStructDef(s) -> CStructDef(s)
      | SExternDecl(x) -> CExternDecl(x)
    in walk l

  in let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
