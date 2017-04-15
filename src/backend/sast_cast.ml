open Sast
open Cast

let sast_to_cast let_decls f_decls =

  let pgn = "gn_" (* gn prefix *)
  in let pgns = "gns_" (* gn struct prefix *)
  in let pkn = "kn_" (* kn prefix *)
  in let plet = "let_" (* let prefix *)

  in let prefix_bind pl = function
    SBind(t, n) -> SBind(t, pl ^ n)

  in let prefix_id pl = function
    | SId(t, n, SLocal) -> SId(t, pl ^ n, SLocal)
    | SId(t, n, SGlobal) -> SId(t, plet ^ n, SGlobal)
    | d -> d

  in let walk_fns f = 

    let walk_stmts p b = 
      let walk_expr = function
        | _ -> [ExprDud]
      in let walk = function
      | _ -> [StmtDud]
      in walk b

    in let walk_gn g = 
      let pgnl = "gnl_" (* gn local prefix *)
      in let pgnx = "gnx_" (* gn exeution state local prefix *)

      in let get_locals f l r =
        List.map (prefix_bind pgnl) (List.map fst f @ List.map fst l @ r)

      in let get_struct n = 
        [SBind(SStruct(pgns ^ n), pgnx)]

      in let rec defn_struct n v =
        let get_val = function
          (SBind(t, s), i) -> SBind(SArray(t, Some(i)), s) (* TODO: struct def'n needs int *)
        in { ssname = pgns ^ n; ssfields = List.map get_val v } 


      in let walk = function { sgname = n; sgret_typ = t; sgformals = f;
                                sglocalvals = ll; sglocalvars = lr;
                                sgbody = b; sgret_expr = r} ->
        [ CStructDef(defn_struct n (f @ ll));
          CFnDecl({ cfname = n; cret_typ = t; cformals = get_struct n; 
                    clocals = get_locals f ll lr; 
                    cbody = let r = walk_stmts pgnl b in r @ walk_stmts pgnl r})]
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
    let interp_expr = function (* this needs to be basically an interpreter *)
      | _ -> StmtDud
    in let walk = function
      | SLetDecl(b, e) -> CConstDecl(b, interp_expr e)
      | SStructDef(s) -> CStructDef(s)
      | SExternDecl(x) -> CExternDecl(x)
    in walk l

  in let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
