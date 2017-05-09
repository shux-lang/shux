open Sast
open Cast

module StringMap = Map.Make(String)

let map_tuple l p =
  let build e = (e, p)
  in List.map build l

let map_opt f = function
  | Some(x) -> Some(f x)
  | None -> None

let styp_of_sexpr = function
  | SLit(t, _) -> t
  | SId(t, _, _) -> t
  | SLookback(t, _, _) -> t
  | SAccess(t, _, _) -> t
  | SBinop(t, _, _, _) -> t
  | SAssign(t, _, _) -> t
  | SKnCall(t, _, _) -> t
  | SGnCall(t, _, _) -> t
  | SLookbackDefault(t, _, _, _) -> t
  | SUnop(t, _, _) -> t
  | SCond(t, _, _, _) -> t
  | SLoopCtr -> assert false (* SInt *)
  | SExprDud -> assert false (* SVoid *)

let sast_to_cast let_decls f_decls =
  let prefix_x s = "extern_" ^ s    (* extern decl *)
  in let prefix_s s = "struct_" ^ s (* struct defn *)
  in let prefix_l s = "let_" ^ s    (* let decl *)
  in let prefix_kn s = "kn_" ^ s    (* kn function *)
  in let prefix_lambda s i = "lambda_" ^ i ^ "_" ^ s
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)

  in let kn_to_fn kn =
    let walk_stmt (e, t) = 
      let rec walk_anon sexpr styp sanon =
        let rec walk_r acc rtyp rexpr =
          let emit t v = (* set sanon register to the value of v *)
            CExpr(t, CAssign(t, sanon, v))
          in let push_anon t e last =  (* push new sanon of type t onto stack, walk e, then do last *)
            CPushAnon(t, CBlock(List.rev (last :: walk_anon e t (CPeekAnon t)))) (* TODO: check order *)
          in let push_anon_nop t e =  (* push new sanon of type t onto stack, walk e *)
            CPushAnon(t, CBlock(List.rev (walk_anon e t (CPeekAnon t)))) (* TODO: check order *)
          in let walk_primitive =
            let lit t l =
              let tr_lit = match l with
                | SLitInt i -> CLitInt i
                | SLitFloat f -> CLitFloat f
                | SLitBool b -> CLitBool b
                | SLitStr s -> CLitStr s
                | _ -> assert false
              in let lit = CLit(t, tr_lit)
              in emit t lit :: acc

            in let id t n =
              let id = CId(t, n)
              in emit t id :: acc

            in let walk_assign t l r =
              let emit_r = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in push_anon t r emit_r :: acc

            in let walk_unop t o e =
              let acc = walk_r acc t e (* leaves sanon register containing result *)
              in let unop = CUnop(t, o, sanon)
              in emit t unop :: acc

            in let walk_binop t l o r =
              let tr_binop = match o with
                (* same type *)
                | SBinopInt o -> CBinopInt o
                | SBinopFloat o -> CBinopFloat o
                | SBinopBool o -> CBinopBool o
                (* change of type *)
                | SBinopPtr o -> CBinopPtr o
                | SBinopGn SDo -> CBinopPtr SIndex (* do x y() := (for x y())[x-1] *)
                | _ -> assert false

              in let primitive = (* operators whose temp value don't change type *)
                let acc = walk_r acc t l (* leaves sanon register coontaining result of l *)
                in let eval_binop = CBinop(t, CPeek2Anon t, tr_binop, CPeekAnon t)
                in let emit_r = CExpr(t, CAssign(t, CPeek2Anon t, eval_binop))
                in push_anon t r emit_r :: acc

              in let dereference = (* operators whose operands are of Array t and int *)
                let eval =  (* TODO: make sure I understand what the fuck is going on here *)
                  let arr_t = styp_of_sexpr l
                  in let ind_t = styp_of_sexpr r
                  in let ind_t = if ind_t=SInt then ind_t else assert false
                  in let eval_deref = CBinop(t, CPeek2Anon arr_t, tr_binop, CPeekAnon ind_t)
                  in let emit_deref = CExpr(t, CAssign(t, CPeek3Anon t, eval_deref))
                  in let eval_r = push_anon ind_t r emit_deref
                  in push_anon arr_t l eval_r
                in eval :: acc

              in match o with
                | SBinopPtr SIndex -> dereference
                | SBinopGn SDo -> assert false (* TODO: see if Mert can sugar this away *)
                | SBinopFn _ -> assert false (* this is inside walk_primitive, arrays not allowed *)
                | SBinopGn _ -> assert false (* see above *)
                | _ -> primitive

            in let walk_access t e s =
              let st_t = styp_of_sexpr e
              in let eval_access = CAccess(t, CPeekAnon st_t, s)
              in let emit_access = CExpr(t, CAssign(t, CPeek2Anon t, eval_access))
              in let eval_struct = push_anon st_t e emit_access
              in eval_struct :: acc

            in let walk_cond t iff the els =
              let cond_t = styp_of_sexpr iff
              in let cond_t = if cond_t=SBool then cond_t else assert false
              in let eval_iff = push_anon_nop cond_t iff
              in let eval_the = push_anon_nop cond_t the
              in let eval_els = push_anon_nop cond_t els
              in let eval_merge = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in let eval_cond = CCond(t, eval_iff, eval_the, eval_els, eval_merge)
              in eval_cond :: acc

            in let walk_call t i a =
              acc

            in match rexpr with
              | SLit(t, l) -> lit t l
              | SId(t, n, _) -> id t n (* don't care about scope *)
              | SUnop(t, o, e) -> walk_unop t o e
              | SBinop(t, l, o, r) -> walk_binop t l o r
              | SAccess (t, e, s) -> walk_access t e s
              | SCond(t, iff, the, els) -> walk_cond t iff the els
              | SKnCall(t, i, a) -> walk_call t i a

              (* requires new nested walk *)
              | SAssign(t, l, r) -> walk_assign t l r 

              (* should never be naked *)
              | SGnCall(_, _, _) -> assert false
              | _ -> assert false

          in let walk_array =
            [ CStmtDud ]

          in let walk_struct =
            [ CStmtDud ]

          in match rtyp with
            | SArray(t, n) -> walk_array
            | SStruct(i, b) -> walk_struct
            | _ -> walk_primitive

        in let walk_l ltyp lexpr =
          let rec lvalue_tr typ ass anon =
             let primitive_assign =
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
              in CLoop(get_cond, lvalue_tr typ get_index get_anon)

            in let struct_assign id binds =
              let map_binds (SBind(t, n, _)) =
                let map_ass a =
                  SAccess(t, a, n)
                in let access_ass = 
                  List.map map_ass ass
                in let access_anon =
                  CAccess(t, anon, n)
                in (t, access_ass, access_anon)
              in let for_each_field =
                List.map map_binds binds
              in let translate (t, ass, anon) =
                lvalue_tr t ass anon
              in let translate_each_field =
                List.map translate for_each_field
              in CBlock translate_each_field

            in match typ with
              | SArray(t, Some n) -> array_assign t n
              | SArray(_, None) -> assert false (* this should not be allowed? *)
              | SStruct(i, b) -> struct_assign i b
              | SPtr | SVoid -> assert false
              | _ -> primitive_assign

          in let rec walk ass = function
            | SAssign(t, l, r) when t=ltyp -> walk (l :: ass) r
            | SAssign(_, _, _) -> assert false
            | e -> lvalue_tr ltyp ass sanon :: walk_r [] ltyp e (* TODO: check order; reversed *)
          in walk [] lexpr

        in walk_l styp sexpr
      in CPushAnon(t, CBlock(List.rev (walk_anon e t (CPeekAnon t)))) (* TODO: check order *)

    in let walk_ret = function
      | Some (e, t) -> CReturn (Some (t, (walk_stmt (e, t)))) 
      | None -> CReturn None

    in let fn_decl kn = CFnDecl 
      { cfname = kn.skname; cret_typ = kn.skret_typ;
        cformals = kn.skformals; clocals = kn.sklocals;
        cbody = List.rev (walk_ret kn.skret_expr :: List.map walk_stmt kn.skbody) }

    in let rec hoist_lambdas kn =
      let hoist n { slret_typ; slformals; sllocals; slbody; slret_expr } = hoist_lambdas
        { skname = prefix_lambda kn.skname n; skret_typ = slret_typ;
          skformals = slformals; sklocals = sllocals; skbody = slbody; 
          skret_expr = slret_expr }

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
