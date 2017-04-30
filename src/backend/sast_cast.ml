open Sast
open Cast

let sast_to_cast let_decls f_decls =

  let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> []
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
