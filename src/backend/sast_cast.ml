open Sast
open Cast

let sast_to_cast let_decls f_decls =
  let prefix_x s = "extern_" ^ s    (* extern decl *)
  in let prefix_s s = "struct_" ^ s (* struct defn *)
  in let prefix_l s = "let_" ^ s    (* let decl *)
  in let prefix_kn s = "kn_" ^ s    (* kn function *)
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)

  in let walk_kn kn =
    let walk = function { skname; _ } ->
    DeclDud 
    in walk kn

  in let walk_gn gn = 
    let prefix_gnv s = "gnv_" ^ s
    in let gns_typ = prefix_gns gn.sgname (* struct type name *)
    in let gns_arg = "gnx_arg"            (* gn execution state argument name *)
    in let gnc = "gnx_ctr"                (* gn execution state counter name *)

    in let gn_to_kn =
      let rec lookback (e, t) = ((match e with
        | SLookback(t, id, n) -> SLit(SInt, SLitInt(42))
        | SAccess(t, e, id) -> SLit(SInt, SLitInt(42))
        | SBinop(t, l, o, r) -> SLit(SInt, SLitInt(42))
        | SAssign(t, l, r) -> SLit(SInt, SLitInt(42))
        | SKnCall(t, id, a) -> SLit(SInt, SLitInt(42))
        | SGnCall(t, id, a) -> SLit(SInt, SLitInt(42))
        | SLookbackDefault(t, n, f, e) -> SLit(SInt, SLitInt(42))
        | SUnop(t, o, e) -> SLit(SInt, SLitInt(42))
        | SCond(t, i, f, e) -> SLit(SInt, SLitInt(42))
        | e -> e), t)

      in { skname = prefix_gn gn.sgname; skret_typ = gn.sgret_typ;
        skformals = [SBind(SStruct(gns_typ), gns_arg, SLocalVar)];
        sklocals = gn.sglocalvars; skbody = List.map lookback gn.sgbody; 
        skret_expr = lookback gn.sgret_expr; }

    in let defn_struct val_binds =
      let val_to_a_decl = function 
        | SBind(t, n, SLocalVal) ->
            SBind(SArray(t, Some(gn.sgmax_iter)), prefix_gnv n, SLocalVar)
        | _ -> raise (Failure "Bad SBind found in sglocalvals") (* TODO: write english *)
      in let ctr_decl =
        SBind(SInt, gnc, SLocalVar)
      in CStructDef({ ssname = gns_typ; 
                      ssfields = ctr_decl :: List.map val_to_a_decl val_binds })

    in [ defn_struct (gn.sgformals @ gn.sglocalvals);
          walk_kn gn_to_kn ]
                            (*
      [ CStructDef(defn_struct n (f @ ll));
        CFnDecl({ cfname = n; cret_typ = t; cformals = get_struct n;
        clocals = get_locals f ll lr;
        cbody = get_ctr :: let br = walk_gn_stmts b m in br @ walk_gn_stmt r})]
        *)
  in let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
      | SKnDecl(k)::t -> let r = walk_kn k in r :: walk t
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
