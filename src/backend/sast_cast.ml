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
  in let prefix_lambda s i = "lambda_" ^ i ^ "_" ^ s
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)

  in let kn_to_fn kn =
    (*
    let rec walk_expr typ expr =
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
      in CExpr(typ, walk expr)

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
        | SAssign(t, l, r) -> walk_l (l :: ass) r
        | e -> walk_r e
      in let num = match num with
        | Some(x) -> CLit(SInt, CLitInt x)
        | None -> assert false
      in CBlock(typ, [])
    and walk_struct typ expr =
      CStmtDud
      (*

    and walk_stmt = function
      | (e, SArray(t, n)) -> walk_loop t n e
      | (e, SStruct(id, _)) -> walk_struct id e
      | (e, SPtr) | (e, SVoid) -> assert false
      | (e, t) -> walk_expr t e
*)
      *)
      (*
    and lduudvalue_tr ass typ =
      let rec tr = function
        | SId(t, n, s) -> CId(t, n)
        | SAccess(t, e, f) -> CAccess(t, tr e, f)
        | SBinop(t, l, SBinopPtr SIndex, r) -> CBinop(t, tr l, CBinopPtr SIndex, tr r)
        | _ -> assert false (* not an l-value *)

      in let unit_assign =
        let fold_ass r l =
          CAssign(typ, tr l, r)
        in [ CExpr(typ, List.fold_left fold_ass (CBlockVal t) ass) ]

      in let array_assign t n v =
        let index_curr t a = CBinop(t, a, CBinopPtr SIndex, CLoopCtr)
        in let get_val = index_curr t v
        in let fold_ass r l =
          CAssign(t, index_curr t (tr l), r)
        in [ CLoop(t, CLit(SInt, (CLitInt n)), CExpr(t, List.fold_left fold_ass get_val ass)) ]

      in let struct_assign i bs =

        (*
         *
         * for each binding:
           * assign each element of Access(ass.member, binding) with Access(CBlockVal, binding)
        let map_ass SBind(t, n, _) =
        in List.map map_ass bs

        let map_ass SBind(ty, n, _) = match ty with
          | SArray(t, Some n) -> array_assign t n (SAccess(ty, CBlockVal (SStruct(i, b)), n)
*)
        [ CStmtDud ]
      in match typ with
        | SArray(t, Some n) -> array_assign t n (CBlockVal t)
        | SArray(_, None) -> assert false (* this should not be allowed? *)
        | SStruct(id, bs) -> struct_assign id bs
        | SPtr | SVoid -> assert false
        | _ -> unit_assign
*)
      (**
    and lvalue_tr ass typ bval = 
      let unit_assign =
        let rec tr = function
          | SId(t, n, s) -> CId(t, n)
          | SAccess(t, e, f) -> CAccess(t, tr e, f)
          | SBinop(t, l, SBinopPtr SIndex, r) -> CBinop(t, tr l, CBinopPtr SIndex, tr r)
          | _ -> assert false (* not an l-value *)
        in let fold_ass rs l =
          CAssign(typ, tr l, rs)
        in [ CExpr(typ, List.fold_left fold_ass bval ass) ]

      in let array_assign t n =
        let index_curr t a = CBinop(t, a, CBinopPtr SIndex, SLoopCtr)
        in let get_val = 
      in match typ with
        | SArray(t, Some n) -> assert false
        | SArray(_, None) -> assert false (* this should not be allowed? *)
        | SStruct(id, bs) -> assert false
        | SPtr | SVoid -> assert false
        | _ -> unit_assign

    and walk_l ass typ = function
      | SAssign(t, l, r) -> walk_l (l :: ass) typ r
      | e -> CBlock(typ, lvalue_tr ass typ (CBlockVal typ) @ walk_m e)
    in let walk_m =
      [ CStmtDud ]
    let rec walk_m expr =
      [CStmtDud]
    *)

    let walk_stmt (e, t) = 
      let walk sexpr styp sanon =
        let walk_m expr =
          [ CStmtDud ]

        in let walk_l typ expr =
          let rec lvalue_tr ass anon =
            let unit_assign =
              let rec tr = function
                | SId(t, n, s) -> CId(t, n)
                | SAccess(t, e, f) -> CAccess(t, tr e, f)
                | SBinop(t, l, SBinopPtr SIndex, r) -> CBinop(t, tr l, CBinopPtr SIndex, tr r)
                | SLoopCtr -> CLoopCtr
                | _ -> assert false (* not an l-value *)
              in let fold_ass rs l =
                CAssign(typ, tr l, rs)
              in CExpr(typ, List.fold_left fold_ass anon ass)

            in let array_assign t n =
              let index t a = CBinop(t, a, CBinopPtr SIndex, CLoopCtr)
              in let get_anon = index t anon
              in let get_cond = CLit(SInt, (CLitInt n))
              in let map_ass a =
                 SBinop(t, a, SBinopPtr SIndex, SLoopCtr)
              in let get_index =
                List.map map_ass ass
              in CLoop(get_cond, lvalue_tr get_index get_anon)

            in let struct_assign id binds =
              CStmtDud
              (*
        let index_curr t a = CBinop(t, a, CBinopPtr SIndex, CLoopCtr)
        in let get_val = index_curr t v
        in let fold_ass r l =
          CAssign(t, index_curr t (tr l), r)
        in [ CLoop(t, CLit(SInt, (CLitInt n)), CExpr(t, List.fold_left fold_ass get_val ass)) ]
*)


            in match typ with
              | SArray(t, Some n) -> array_assign t n
              | SArray(_, None) -> assert false (* this should not be allowed? *)
              | SStruct(i, b) -> struct_assign i b
              | SPtr | SVoid -> assert false
              | _ -> unit_assign
          in let rec walk ass = function
            | SAssign(t, l, r) -> walk (l :: ass) r
            | e -> lvalue_tr ass sanon :: walk_m e
          in walk [] expr

        in walk_l styp sexpr
      in CPushAnon(t, CBlock(walk e t (CPeekAnon t)))

    in let walk_ret = function
      | Some x -> [] 
      | None -> []

    in let fn_decl kn = CFnDecl 
      { cfname = kn.skname; cret_typ = kn.skret_typ;
        cformals = kn.skformals; clocals = kn.sklocals;
        cbody = List.map walk_stmt kn.skbody @ walk_ret kn.skret_expr }

    in let rec hoist_lambdas kn =
      let hoist n { slret_typ; slformals; sllocals; slbody; slret_expr } = hoist_lambdas
        { skname = prefix_lambda kn.skname n; skret_typ = slret_typ;
          skformals = slformals; sklocals = sllocals; skbody = slbody; 
          skret_expr = Some slret_expr }

      in let rec fish acc p = function
        | SLit(_, SLitKn(l)) -> (hoist p l) :: acc
        | SBinop(_, l, _, r) -> fish [] (p ^ "l") l @ fish acc (p ^ "r") r
        | SAssign(_, l, r) -> fish [] (p ^ "l") l @ fish acc (p ^ "r") r
        | SCond(_, i, t, e) -> fish [] (p ^ "i") i @ fish [] (p ^ "t") t @ fish acc (p ^ "e") e
        | SUnop(_, _, e) -> fish acc (p ^ "u") e
        | SKnCall(_, _, a) -> (List.concat (List.map (fish [] (p ^ "k")) (List.map fst a))) @ acc
        | SGnCall(_, _, a) -> (List.concat (List.map (fish [] (p ^ "g")) (List.map fst a))) @ acc
        | SAccess(_, e, _) -> fish acc (p ^ "a") e
        | _ -> acc
      in let rec bait p = function
        | SLit(t, SLitKn(l)) -> SId(t, prefix_lambda kn.skname p, SKnCall) (* should this be a global here? *)
        | SBinop(t, l, o, r) -> SBinop(t, bait (p ^ "l") l, o, bait (p ^ "r") r)
        | SAssign(t, l, r) -> SAssign(t, bait (p ^ "l") l, bait (p ^ "r") r)
        | SCond(ty, i, t, e) -> SCond(ty, bait (p ^ "i") i, bait (p ^ "t") t, bait (p ^ "e") e)
        | SUnop(t, o, e) -> SUnop(t, o, bait (p ^ "u") e)
        | SKnCall(t, s, a) -> SKnCall(t, s, List.map (fun (a, t) -> (bait (p ^ "k") a, t)) a)
        | SGnCall(t, s, a) -> SGnCall(t, s, List.map (fun (a, t) -> (bait (p ^ "g") a, t)) a)
        | SAccess(t, e, s) -> SAccess(t, bait (p ^ "a") e, s)
        | s -> s

      in let walk_stmt (lambdas, body, p) (e, t) =
        (fish lambdas p e, ((bait p e), t) :: body, p ^ "x")
      in let (lambdas, body, _) = List.fold_left walk_stmt ([], [], "z") kn.skbody
      in let body = List.rev body
      in List.rev (fn_decl {kn with skbody = body} :: List.concat lambdas)
    in hoist_lambdas kn

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
          | SStructField | SKnCall -> assert false

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

    in defn_cstruct :: kn_to_fn gn_to_kn

  in let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
      | SKnDecl(k)::t -> let r = walk_kn k in r @ walk t
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
