open Sast
open Cast

module StringMap = Map.Make(String)

let map_tuple l p =
  let build e = (e, p)
  in List.map build l

let map_opt f = function
  | Some(x) -> Some(f x)
  | None -> None

let sast_to_cast let_decls f_decls =
  let prefix_x s = "extern_" ^ s    (* extern decl *)
  in let prefix_s s = "struct_" ^ s (* struct defn *)
  in let prefix_l s = "let_" ^ s    (* let decl *)
  in let prefix_kn s = "kn_" ^ s    (* kn function *)
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)

  in let kn_to_fn kn =
    let rec walk_expr e typ =
      let lit = function
        | SLitInt(i) -> CLitInt(i)
        | SLitFloat(f) -> CLitFloat(f)
        | SLitBool(b) -> CLitBool(b)
        | SLitStr(s) -> CLitStr(s)
        | _ -> assert false
      in let binop = function
        | SBinopInt(o) -> CBinopInt(o)
        | SBinopFloat(o) -> CBinopFloat(o)
        | SBinopBool(o) -> CBinopBool(o)
        | SBinopPtr(o) -> CBinopPtr(o)
        | _ -> assert false
      in let rec walk = function
        | SLit(t, l) -> CLit(t, lit l)
        | SId(t, id, _) -> CId(t, id)
        | SAccess(t, e, id) -> CAccess(t, walk e, id)
        | SBinop(t, l, o, r) -> CBinop(t, walk l, binop o, walk r)
        | SAssign(t, l, r) -> CAssign(t, walk l, walk r)
        | SKnCall(t, id, a) -> CCall(t, id, List.map walk_stmt a)
        | SUnop(t, o, e) -> CUnop(t, o, walk e)
        | SCond(t, i, f, e) -> CCond(t, walk i, walk f, walk e)
        | _ -> assert false
      in CExpr(typ, walk e)

    and walk_loop typ num expr =
      let lit = function
        | SLitArray(es) -> CLitArray(List.map walk_stmt (map_tuple es typ))
        | _ -> assert false
      in let binop = function
        | SBinopFn(o) -> ()
        | _ -> assert false
      in let rec walk_r = function
        | SLit(t, l) -> CLit(t, lit l)              (* r-value *)
        | SId(t, id, _) -> CId(t, id)               (* lr-value *)
        | SBinop(t, l, o, r) -> assert false        (* r-value *)
        | SKnCall(t, id, a) -> assert false         (* r-value *)
        | SCond(t, i, f, e) -> assert false         (* r-value *)
        | SAssign(t, l, r) -> assert false          (* m-value *)
        | _ -> CExprDud
      in let rec walk_l ass = function
        | SAssign(t, l, r) -> walk_l ((walk_expr l)::ass) r
        | e -> walk_r e
      in let num = match num with
        | Some(x) -> CLit(SInt, CLitInt x)
        | None -> assert false
      in CLoop(typ, num, CExpr (SPtr, CExprDud))
   (*   in CExpr(typ, walk_l [] expr) *)

    and walk_struct typ expr =
      CStmtDud
    and walk_stmt = function
      | (e, SArray(t, n)) -> walk_loop t n e
      | (e, SStruct(id, _)) -> walk_struct id e
      | (e, SPtr) -> assert false
      | (e, SVoid) -> assert false
      | (e, t) -> walk_expr e t

    in let walk_ret = function
      | Some(r, t) -> [CReturn(t, Some (walk_stmt (r, t)))]
      | None -> []

    in CFnDecl { cfname = kn.skname; cret_typ = kn.skret_typ;
                  cformals = kn.skformals;
                  clocals = kn.sklocals;
                  cbody = List.map walk_stmt kn.skbody @ walk_ret kn.skret_expr }

  in let walk_kn kn =
     kn_to_fn {kn with skname = prefix_kn kn.skname }

  in let walk_gn gn = 
    let prefix_gnv s = "gnv_" ^ s         (* for local vars *)
    in let gns_arg = "gnx_arg"            (* gn execution state argument name *)
    in let gnc = "gnx_ctr"                (* gn execution state counter name *)

    in let st_fields =
      let a_decl = function
        | SBind(t, n, SLocalVal) -> SBind(SArray(t, Some gn.sgmax_iter), n, SStructField)
        | SBind(_, n, _)-> assert false
      in let ctr_decl =
        SBind(SInt, gnc, SLocalVar)
      in ctr_decl :: List.map a_decl (gn.sgformals @ gn.sglocalvals)

    in let gns_typ_name = prefix_gns gn.sgname
    in let gns_typ = SStruct(gns_typ_name, st_fields) (* struct type name *)
    in let defn_cstruct = CStructDef { ssname = gns_typ_name; ssfields = st_fields }

    in let gn_to_kn =
      let st_id = SId(gns_typ, gns_arg, SLocalVar)
      in let st_element t id = SAccess(t, st_id, id)
      in let st_var t = st_element (SArray(t, Some gn.sgmax_iter))
      in let st_cnt = st_element SInt gnc
      in let wrap_int n = SLit(SInt, SLitInt n)

      in let prefix_var = function
        | SBind(t, n, SLocalVar) -> SBind(t, prefix_gnv n, SLocalVar)
        | SBind(_, n, _)-> assert false

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
          | SStructField -> assert false

        in let rec lb = function
          | SId(t, id, s) -> sid t id s
          | SLookback(t, id, n) -> lb_st t id n
          | SLookbackDefault(t, n, f, e) -> SCond(t, lb_cmp n, lb f, lb e)
          | SAccess(t, e, id) -> SAccess(t, lb e, id)
          | SBinop(t, l, o, r) -> SBinop(t, lb l, o, lb r)
          | SAssign(t, l, r) -> SAssign(t, lb l, lb r)
          | SKnCall(t, id, a) -> SKnCall(t, id, List.map lookback a)
          | SGnCall(t, id, a) -> SGnCall(t, id, List.map lookback a)
          | SUnop(t, o, e) -> SUnop(t, o, lb e)
          | SCond(t, i, f, e) -> SCond(t, lb i, lb f, lb e)
          | e -> e
        in (lb e, t)
      in { skname = prefix_gn gn.sgname; skret_typ = gn.sgret_typ;
        skformals = [ SBind(gns_typ, gns_arg, SLocalVar) ];
        sklocals = List.map prefix_var gn.sglocalvars; 
        skbody = inc_cnt :: List.map lookback gn.sgbody; 
        skret_expr = map_opt lookback gn.sgret_expr }

    in [ defn_cstruct; kn_to_fn gn_to_kn ]

  in let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
      | SKnDecl(k)::t -> let r = walk_kn k in r :: walk t
    in walk f_decls
  in let walk_static let_decls =
    let interp_expr = function (* TODO: write interpretor for compile-time evaluation *)
      | _ -> CStmtDud
    in let walk = function
      | SLetDecl(SBind(t, n, s), e) -> CConstDecl(SBind(t, prefix_l n, s), interp_expr e)
      | SStructDef s -> CStructDef {s with ssname = prefix_s s.ssname}
      | SExternDecl x -> CExternDecl {x with sxalias = prefix_x x.sxalias}
    in walk let_decls

  (* function entry point: walk entire program *)
  in let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
