open Printf
open Dba_types
open Yojson.Basic.Util


(* exceptions *)
exception Unhandled of string


(* utils *)
let wrap t st args = `Assoc [
    ("Type", `String t) ;
    ("SubType", `String st) ;
    ("Args", `List args)
  ]


(* translators *)
let json_endian endian =
  match endian with
  | Dba.LittleEndian -> wrap "Endian" "LittleEndian" []
  | Dba.BigEndian -> wrap "Endian" "BigEndian" []

let json_size size =
  `Int size (* TODO *)
  (* `Int (Basic_types.BitSize.to_int size) *)

let json_unop op =
  let wrap t = "UnOp", (wrap "UnOpKind" t []) in

  match op with
  | Dba.UMinus -> wrap "NEG"
  | Dba.Not -> wrap "NOT"

let json_binop op =
  let wrap_bin t = "BinOp", (wrap "BinOpKind" t []) in
  let wrap_rel t = "RelOp", (wrap "RelOpKind" t []) in

  match op with
  | Dba.Plus -> wrap_bin "ADD"
  | Dba.Minus -> wrap_bin "SUB"
  | Dba.MultU -> wrap_bin "MUL"
  | Dba.MultS -> raise (Unhandled "MultS") (* TODO *)
  | Dba.DivU -> wrap_bin "DIV"
  | Dba.DivS -> wrap_bin "SDIV"
  | Dba.ModU -> wrap_bin "MOD"
  | Dba.ModS -> wrap_bin "SMOD"
  | Dba.Or -> wrap_bin "OR"
  | Dba.And -> wrap_bin "AND"
  | Dba.Xor -> wrap_bin "XOR"
  | Dba.Concat -> wrap_bin "CONCAT"
  | Dba.LShift -> wrap_bin "SHL"
  | Dba.RShiftU -> wrap_bin "SHR"
  | Dba.RShiftS -> wrap_bin "SAR"
  | Dba.LeftRotate -> raise (Unhandled "LeftRotate")
  | Dba.RightRotate -> raise (Unhandled "RightRotate")
  | Dba.Eq -> wrap_rel "EQ"
  | Dba.Diff -> raise (Unhandled "Diff")
  | Dba.LeqU -> wrap_rel "LE"
  | Dba.LtU -> wrap_rel "LT"
  | Dba.GeqU -> raise (Unhandled "GeqU")
  | Dba.GtU -> raise (Unhandled "GtU")
  | Dba.LeqS -> wrap_rel "SLE"
  | Dba.LtS -> wrap_rel "SLT"
  | Dba.GeqS -> raise (Unhandled "GeqS")
  | Dba.GtS -> raise (Unhandled "GtS")

let rec json_expr expr =
  let wrap st args = wrap "Expr" st args in

  match expr with
  | Dba.ExprVar (_, _, _) -> wrap "TODO ExprVar" []
  | Dba.ExprLoad (s, endian, e) ->
      wrap "Load" [json_expr e ; json_endian endian ; json_size s]

  | Dba.ExprCst (_, _) -> wrap "TODO ExprCst" []
  | Dba.ExprUnary (op, e) -> begin
      let op_s, op_json = json_unop op in
      wrap op_s [op_json ; json_expr e]
    end

  | Dba.ExprBinary (op, e1, e2) -> begin
      match op with
      | Dba.Diff -> json_expr (Expr.binary Dba.Minus e1 e2) (* TODO with if-then-else *)
      | Dba.GtU -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LeqU e1 e2))
      | Dba.GeqU -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LtU e1 e2))
      | Dba.GtS -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LeqS e1 e2))
      | Dba.GeqS -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LtS e1 e2))
      | _ ->
          let op_s, op_json = json_binop op in
          wrap op_s [op_json ; json_expr e1 ; json_expr e2]
    end

  | Dba.ExprRestrict (_, _, _) -> wrap "TODO ExprRestrict" []
  | Dba.ExprExtU (_, _) -> wrap "TODO ExprExtU" []
  | Dba.ExprExtS (_, _) -> wrap "TODO ExprExtS" []
  | Dba.ExprIte (_, _, _) -> wrap "TODO ExprIte" []
  | Dba.ExprAlternative (_, _) -> wrap "TODO ExprAlternative" []

let json_compare compare =
  match compare with
  | Dba.FlgCmp (_, _) -> wrap "Compare" "TODO FlgCmp" []
  | Dba.FlgSub (_, _) -> wrap "Compare" "TODO FlgSub" []
  | Dba.FlgTest (_, _) -> wrap "Compare" "TODO FlgTest" []
  | Dba.FlgUnspecified -> wrap "Compare" "TODO FlgUnspecified" []

let json_vartag vartag =
  match vartag with
  | Dba.Flag (_) -> wrap "Vartag" "TODO Flag" []
  | Dba.Temp -> wrap "Vartag" "TODO Temp" []

let json_lhs lhs =
  match lhs with
  | Dba.LhsVar (_, _, _) -> wrap "Lhs" "TODO LhrVar" []
  | Dba.LhsVarRestrict (_, _, _, _) -> wrap "Lhs" "TODO LhsVarRestrict" []
  | Dba.LhsStore (_, _, _) -> wrap "Lhs" "TODO LhrStore" []

let json_stmt s =
  let wrap st args = wrap "Stmt" st args in

  match s with
  | Dba.IkAssign (lhs, expr, _) -> wrap "Move" [json_lhs lhs ; json_expr expr]
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
  | Dba.IkPrint (_, _) -> raise (Unhandled "IkPrint")

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