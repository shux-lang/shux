open Sast
open Cast

let sast_to_cast let_decls f_decls =
  let prefix_x s = "extern_" ^ s    (* extern decl *)
  in let prefix_s s = "struct_" ^ s (* struct defn *)
  in let prefix_l s = "let_" ^ s    (* let decl *)
  in let prefix_kn s = "kn_" ^ s    (* kn function *)
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)

  in let kn_to_fn kn =
    let rec walk = function (* TODO: this is gonna be fed a tuple of typ * expr *)
      | SLit(t, l) -> []
      | SId(t, id, s) -> []
      | SAccess(t, e, id) -> []
      | SBinop(t, l, o, r) -> []
      | SAssign(t, l, r) -> []
      | SKnCall(t, id, e) -> []
      | SGnCall(t, id, e) -> []
      | SUnop(t, o, e) -> []
      | SCond(t, i, f, e) -> []
      | SLookbackDefault(_) -> raise (Failure ("Tried to lookback default in kn: " ^ kn.skname))
      | SLookback(_, id, _) -> raise (Failure ("Tried to lookback " ^ id ^ " in kn: " ^ kn.skname))
    in let walk_loop typ num = function
      | _ -> []
    in let rec walk_block = function
      | [] -> []
      | (e, SArray(t, n))::ll -> let r = walk_loop t n e in r @ walk_block ll
      | (e, SStruct(id))::ll -> [] (* TODO: struct assignment? *)
      | (e, SPtr)::ll -> raise (Failure "Tried to walk a SPtr type expr")
      | (e, SVoid)::ll -> raise (Failure "Tried to walk a SVoid type expr")
      | (e, _)::ll -> let r = CBlock(walk e) in r :: walk_block ll
    in let walk_ret = function
      | _ -> []
    in CFnDecl { cfname = kn.skname; cret_typ = kn.skret_typ;
                  cformals = kn.skformals;
                  clocals = kn.sklocals;
                  cbody = walk_block kn.skbody @ walk_ret kn.skret_expr }

  in let walk_kn kn =
     kn_to_fn {kn with skname = prefix_kn kn.skname }

  in let walk_gn gn = 
    let prefix_gnv s = "gnv_" ^ s         (* for local vars *)
    in let gns_typ = prefix_gns gn.sgname (* struct type name *)
    in let gns_arg = "gnx_arg"            (* gn execution state argument name *)
    in let gnc = "gnx_ctr"                (* gn execution state counter name *)

    in let gn_to_kn =
      let st_id = SId(SStruct gns_typ, gns_arg, SLocalVar)
      in let st_element t id = SAccess(t, st_id, id)
      in let st_var t = st_element (SArray(t, Some gn.sgmax_iter))
      in let st_cnt = st_element SInt gnc
      in let wrap_int n = SLit(SInt, SLitInt n)

      in let prefix_var = function
        | SBind(t, n, SLocalVar) -> SBind(t, prefix_gnv n, SLocalVar)
        | SBind(_, n, _)-> raise (Failure ("Bad SBind found in sglocalvars: " ^ n))

      in let inc_cnt =
        let t = SInt
        in let inc = SBinop(t, st_cnt, SBinopInt SAddi, wrap_int 1)
        in let e = SAssign(t, st_cnt, inc)
        in (e, t)

      in let lb_st t id n =
        (* should be gnx_arg.id[(gnx_ctr - n) % mod_iter] *)
        let idx = SBinop(SInt, wrap_int n, SBinopInt SSubi, st_cnt)
        in let idx = SBinop(SInt, idx, SBinopInt SMod, wrap_int gn.sgmax_iter)
        in SBinop(t, st_var t id, SBinopPtr SIndex, idx)

      in let lb_cmp n = SBinop(SBool, wrap_int n, SBinopInt SLeqi, st_cnt)

      in let rec lookback (e, t) =
        let sid t id = function
          | SGlobal as s -> SId(t, id, s) (* global prefixing will happen in walk_kn *)
          | SLocalVar as s -> SId(t, prefix_gnv id, s)
          | SLocalVal -> lb_st t id 0

        in let rec lb = function
          | SId(t, id, s) -> sid t id s
          | SLookback(t, id, n) -> lb_st t id n
          | SLookbackDefault(t, n, f, e) -> SCond(t, lb_cmp n, lb f, lb e)
          | SAccess(t, e, id) -> SAccess(t, lb e, id)
          | SBinop(t, l, o, r) -> SBinop(t, lb l, o, lb r)
          | SAssign(t, l, r) -> SAssign(t, lb l, lb r)
          | SKnCall(t, id, a) -> SKnCall(t, id, List.map lb a)
          | SGnCall(t, id, a) -> SGnCall(t, id, List.map lb a)
          | SUnop(t, o, e) -> SUnop(t, o, lb e)
          | SCond(t, i, f, e) -> SCond(t, lb i, lb f, lb e)
          | e -> e
        in (lb e, t)

      in { skname = prefix_gn gn.sgname; skret_typ = gn.sgret_typ;
        skformals = [ SBind(SStruct gns_typ, gns_arg, SLocalVar) ];
        sklocals = List.map prefix_var gn.sglocalvars; 
        skbody = inc_cnt :: List.map lookback gn.sgbody; 
        skret_expr = lookback gn.sgret_expr; }

    in let defn_struct val_binds =
      let a_decl = function 
        | SBind(t, n, SLocalVal) -> SBind(SArray(t, Some gn.sgmax_iter), n, SLocalVar)
        | SBind(_, n, _)-> raise (Failure ("Bad SBind found in sglocalvals: " ^ n))
      in let ctr_decl =
        SBind(SInt, gnc, SLocalVar)
      in CStructDef { ssname = gns_typ; 
                      ssfields = ctr_decl :: List.map a_decl val_binds }

    in [ defn_struct (gn.sgformals @ gn.sglocalvals); kn_to_fn gn_to_kn ]

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
      | SStructDef s -> CStructDef {s with ssname = prefix_s s.ssname}
      | SExternDecl x -> CExternDecl {x with sxalias = prefix_x x.sxalias}
    in walk let_decls
  (* function entry point: walk entire program *)
  in 
  let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
