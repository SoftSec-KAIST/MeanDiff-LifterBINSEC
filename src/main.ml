open Printf;;

let lift raw = Decode_utils.decode_hex_opcode raw

let _ =
  (* comment out to hide debug output *)
  Logger.set_log_level "debug";

  let opc, dba = lift Sys.argv.(1) in

  (* print dba *)
  Logger.debug "\n\n%s\n==================================================================" opc;
  Dba_types.Block.iter (fun i -> Logger.debug "%a" Dba_printer.Ascii.pp_instruction i;) dba;
  Logger.debug "\n\n";

