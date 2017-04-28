(* exceptions *)
exception Unhandled_Instr of string


(* translate single instruction *)
let transInstr i =
  match i with
  | Dba.IkAssign (_, _, _) -> "TODO IkAssign"
  | Dba.IkSJump (_, _) -> "TODO IkSJump"
  | Dba.IkDJump (_, _) -> "TODO IkDJump"
  | Dba.IkIf (_, _, _) -> "TODO IfIf"
  | Dba.IkStop (_) -> "TODO IkStop"
  | Dba.IkAssert (_, _) -> "TODO IkAssert"
  | Dba.IkAssume (_, _) -> "TODO IkAssume"
  | Dba.IkNondetAssume (_, _, _) -> "TODO IkNondetAssume"
  | Dba.IkNondet (_, _, _) -> "TODO IkNondet"
  | Dba.IkUndef (_, _) -> "TODO IkUndef"
  | Dba.IkMalloc (_, _, _) -> "TODO IkMalloc"
  | Dba.IkFree (_, _) -> "TODO IkFree"
  | Dba.IkPrint (_, _) -> raise (Unhandled_Instr "IkPrint")

let _ =
  (* comment out to hide debug output *)
  Logger.set_log_level "debug";

  (* lift instruction *)
  let opc, dba = Decode_utils.decode_hex_opcode Sys.argv.(1) in

  (* print dba *)
  Logger.debug "\n\n%s\n==================================================================" opc;
  Dba_types.Block.iter (fun i -> Logger.debug "%a" Dba_printer.Ascii.pp_instruction i;) dba;
  Logger.debug "\n\n";

  (* translate into list of strings *)
  let res = Dba_types.Block.fold_left (fun l i -> (transInstr i) :: l) [] dba in

  (* print the list, line by line *)
  Logger.debug "\n==================================================================";
  List.iter print_endline res
