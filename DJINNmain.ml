open Format
open Lexing
open DJINNparser

let usage = "usage: natrix [options] file.nx"

let parse_only = ref false
let compiler_only = ref false
let interpreter_only = ref false

let ofile = ref "test.s"

let set_file f s = f := s 

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--compiler-only", Arg.Set compiler_only, " only generate compiler";
    "--interpreter-only", Arg.Set interpreter_only, " only  interpreter";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".nx") then
      raise (Arg.Bad "no .nx extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in  (*   lb = Lexing buffer   *)
  try
    let f = DJINNparser.prog DJINNlexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;
    if !interpreter_only then (DJINNinterp.prog f; exit 0);
    if !compiler_only then (DJINNcompile.compile_program f !ofile; exit 0);
    DJINNinterp.prog f;
    DJINNcompile.compile_program f !ofile
  with
    | DJINNlexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | DJINNparser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | DJINNinterp.Error s ->
	eprintf "error: %s@." s;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
