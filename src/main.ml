open Printf
open Yojson.Basic.Util


(* exceptions *)
exception Unhandled_Instr of string


(* utils *)
let wrap t st args = `Assoc [
    ("Type", `String t) ;
    ("SubType", `String st) ;
    ("Args", `List args)
  ]


(* translators *)
let json_expr expr = wrap "Expr" "TODO" []

let json_stmt s =
  let wrap st args =
    `Assoc [
      ("Type", `String "Stmt") ;
      ("SubType", `String st) ;
      ("Args", `List args)
    ] in

  match s with
  | Dba.IkAssign (lhs, expr, _) -> wrap "Move" [json_expr lhs ; json_expr expr]
  | Dba.IkSJump (_, _) -> wrap "TODO IkSJump" []
  | Dba.IkDJump (_, _) -> wrap "TODO IkDJump" []
  | Dba.IkIf (_, _, _) -> wrap "TODO IkIf" []
  | Dba.IkStop (_) -> wrap "TODO IkStop" []
  | Dba.IkAssert (_, _) -> wrap "TODO IkAssert" []
  | Dba.IkAssume (_, _) -> wrap "TODO IkAssume" []
  | Dba.IkNondetAssume (_, _, _) -> wrap "TODO IkNondetAssume" []
  | Dba.IkNondet (_, _, _) -> wrap "TODO IkNondet" []
  | Dba.IkUndef (_, _) -> wrap "TODO IkUndef" []
  | Dba.IkMalloc (_, _, _) -> wrap "TODO IkMalloc" []
  | Dba.IkFree (_, _) -> wrap "TODO IkFree" []
  | Dba.IkPrint (_, _) -> raise (Unhandled_Instr "IkPrint")

let json_ast addr len dba =
  let imm = wrap "Imm" "Integer" [`Int (addr + len) ; `Int 32] in
  let num = wrap "Expr" "Num" [imm] in
  let stmts = Dba_types.Block.fold_left (fun l i -> (json_stmt i) :: l) [] dba in
  let rev_ast =
    match stmts with
    | [] -> [wrap "Stmt" "End" [num]]
    | (`Assoc [("Type", `String "Stmt") ; ("SubType", `String "End") ; _]) :: _ -> stmts
    | _ :: _ -> (wrap "Stmt" "End" [num]) :: stmts
  in
  wrap "AST" "Stmts" (List.rev rev_ast)


(* main *)
let _ =
  (* comment out to hide debug output *)
  Logger.set_log_level "debug";

  (* lift instruction *)
  let opc, dba = Decode_utils.decode_hex_opcode Sys.argv.(1) in

  (* print dba *)
  Logger.debug "\n\n%s\n==================================================================" opc;
  Dba_types.Block.iter (fun i -> Logger.debug "%a" Dba_printer.Ascii.pp_instruction i;) dba;
  Logger.debug "\n\n";

  (* variables *)
  let addr = 0x8048000 in
  let len = (Dba_types.Block.length dba) * 32 in

  (* translate into json *)
  let json = json_ast addr len dba in

  (* print the list, line by line *)
  Logger.debug "\n==================================================================";
  printf "%s\n" (Yojson.Basic.pretty_to_string json)
