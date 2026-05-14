open StdLabels
open Bos
open Cmdliner

let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let split_error_message message =
  let splitted = String.split_on_char ~sep:'\n' message in
  match splitted with
  | [] -> ("syntax error", [])
  | [primary] -> (primary, [])
  | primary::secondary ->
    let secondary =
      List.map secondary
      ~f:(fun secondary ->
          if String.starts_with secondary ~prefix:"Note: "
          then String.sub secondary ~pos:6 ~len:(String.length secondary - 6)
          else secondary)
      |> List.filter ~f:(fun secondary -> if String.trim secondary = "" then false else true) in
    (primary, secondary)

let parse_file path =
  let open Result.Syntax in
  let* content = OS.File.read path in
  let name = Fpath.to_string path in
  let source : Grace.Source.t = `File name in
  let lexbuf = Lexing.from_string content in
  Lexing.set_filename lexbuf name;
  match (Liss.parse_lexbuf lexbuf, content) with
  | program -> Ok program
  | exception Liss.Parse_error (message, (pos_start, pos_end)) ->
    let range = Grace.Range.create ~source
      (Grace.Byte_index.of_lex pos_start) 
      (Grace.Byte_index.of_lex pos_end) in
    let (primary, notes) = split_error_message message in
    let notes = List.map notes ~f:Grace.Diagnostic.Message.create in
    let primary_label =
      Grace.Diagnostic.Message.create primary
      |> Grace.Diagnostic.Label.primary ~range in
    let diagnostic =
      Grace.Diagnostic.createf Grace.Diagnostic.Severity.Error "Syntax error during verification"
        ~labels:[primary_label] ~notes in
    let diagnotic_pp = Grace_ansi_renderer.pp_diagnostic
      ~code_to_string:(fun _ -> "")
      ~config:(Grace_ansi_renderer.Config.default) in
    diagnotic_pp Format.err_formatter diagnostic;
    Error (`Msg "")

let get_line_num (vc : Wp.vc) =
  let start, _ = vc.loc in
  start.pos_lnum

let check_and_output program show_goals =
  let vcs = Wp.verify program in
  let verdicts =
    List.map vcs ~f:(fun (vc : Wp.vc) ->
        (vc, Smt.Solver.check_alt_ergo vc.goal))
  in
  let num_verdicts = List.length verdicts in
  let succeeded = ref 0 in
  let failed = ref 0 in
  List.iteri verdicts ~f:(fun i ((vc : Wp.vc), result) ->
      Fmt.pf Format.std_formatter "[%d/%d] %a (on line %d): " (i + 1)
        num_verdicts Wp.pp_kind vc.kind (get_line_num vc);
      match (result : (Smt.Solver.smt_result, [ `Msg of string ]) result) with
      | Ok Valid ->
          incr succeeded;
          Fmt.string Format.std_formatter "valid\n";
          if show_goals then
            Fmt.pf Format.std_formatter "Goal: %a\n" Formula.pp vc.goal
      | Ok Invalid ->
          incr failed;
          Fmt.string Format.std_formatter "invalid\n";
          Fmt.pf Format.std_formatter "Goal: %a\n" Formula.pp vc.goal
      | Ok (Unknown _) ->
          incr failed;
          Fmt.string Format.std_formatter "unknown\n";
          Fmt.pf Format.std_formatter "Goal: %a\n" Formula.pp vc.goal
      | Error (`Msg error_message) ->
          incr failed;
          Fmt.string Format.std_formatter "\n";
          Fmt.pf Format.err_formatter "\nError: %s\n" error_message);
  Fmt.pf Format.std_formatter "%d total, %d succeeded, %d failed\n" num_verdicts
    !succeeded !failed

let parse_and_verify file show_goals =
  match parse_file file with
  | Ok (program, _content) -> Ok (check_and_output program show_goals)
  | Error _ as err -> err

let program_arg =
  let doc = "path to a liss program" in
  Arg.(required & pos 0 (some fpath) None & info [] ~docv:"FILE" ~doc)

let show_goals_arg =
  let doc = "show all goals" in
  Arg.(value & flag & info [ "g"; "goals" ] ~doc)

let cmd =
  let doc = "Verify a liss program" in
  let info = Cmd.info "liss-verify" ~version:"0.1.0" ~doc in
  Cmd.v info
    Term.(term_result (const parse_and_verify $ program_arg $ show_goals_arg))

let () = exit (Cmd.eval cmd)
