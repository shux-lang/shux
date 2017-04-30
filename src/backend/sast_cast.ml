open Sast
open Cast

let sast_to_cast let_decls f_decls =
  let walk_gn gn = 
    let gn_struct_id = "gns_" (* gn struct prefix *)
    in let pgnc = "gnc_" (* gn execution state counter prefix *)
    in let defn_struct name val_binds max_iter =
      let val_to_array_decl = function SBind(t, s)
        -> SBind(SArray(t, Some(max_iter)), "" ^ s)
      in let ctr_decl =
        SBind(SInt, pgnc)
      in SStructDef({ ssname = ""; ssfields = ctr_decl :: List.map val_to_array_decl val_binds })

    in let walk = function { sgname = n; sgret_typ = t; sgmax_iter = m;
                              sgformals = f; sglocalvals = ll; sglocalvars = lr;
                              sgbody = b; sgret_expr = r}
      -> [ ]
                            (*
      [ CStructDef(defn_struct n (f @ ll));
        CFnDecl({ cfname = n; cret_typ = t; cformals = get_struct n;
        clocals = get_locals f ll lr;
        cbody = get_ctr :: let br = walk_gn_stmts b m in br @ walk_gn_stmt r})]
        *)
    in walk gn
  in let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
      | SKnDecl(k)::t -> []
    in walk f_decls
  in let walk_static let_decls =
    let interp_expr = function (* TODO: write interpretor for compile-time evaluation *)
      | _ -> StmtDud
    in let walk = function
      | SLetDecl(b, e) -> CConstDecl(b, interp_expr e)
      | SStructDef(s) -> CStructDef(s)
      | SExternDecl(x) -> CExternDecl(x)
    in walk let_decls
  (* function entry point: walk entire program *)
  in let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
