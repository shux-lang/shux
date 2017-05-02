open Sast
open Cast

let sast_to_cast let_decls f_decls =
  let prefix_x s = "extern_" ^ s
  in let prefix_s s = "struct_" ^ s 
  in let prefix_l s = "let_" ^ s
  in let prefix_kn s = "kn_" ^ s
  in let prefix_gn s = "gn_" ^ s
  in let prefix_gns s = "gns_" ^ s (* gn struct *)

  in let walk_gn gn = 
    let gn_struct_id = "gns_" (* gn struct prefix *)
    in let pgnc = "gnc_" (* gn execution state counter prefix *)

    in let defn_struct val_binds max_iter =
      let val_to_a_decl = function 
        | SBind(t, s, SLocalVal) -> SBind(SArray(t, Some(max_iter)), s, SLocalVar)
        | _ -> raise (Failure "shit")
      in let ctr_decl =
        SBind(SInt, pgnc, SLocalVar)
      in CStructDef({ ssname = prefix_gns gn.sgname; 
                      ssfields = ctr_decl :: List.map val_to_a_decl val_binds })
    in let walk { sgname; sgret_typ; sgmax_iter; 
                  sgformals; sglocalvals; sglocalvars;
                  sgbody; sgret_expr } =
      [ defn_struct (sgformals @ sglocalvals) sgmax_iter ]
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
    in
    let walk = function
      | SLetDecl(SBind(t, n, s), e) -> CConstDecl(SBind(t, prefix_l n, s), interp_expr e)
      | SStructDef(s) -> CStructDef({s with ssname = prefix_s s.ssname})
      | SExternDecl(x) -> CExternDecl({x with sxalias = prefix_x x.sxalias})
    in walk let_decls
  (* function entry point: walk entire program *)
  in 
  let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
