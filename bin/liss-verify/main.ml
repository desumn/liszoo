open StdLabels
open Bos
open Cmdliner

let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let split_error_message message =
  let splitted = String.split_on_char ~sep:'\n' message in
  match splitted with
  | [] -> ("syntax error", [])
  | [ primary ] -> (primary, [])
  | primary :: secondary ->
      let secondary =
        List.map secondary ~f:(fun secondary ->
            if String.starts_with secondary ~prefix:"Note: " then
              String.sub secondary ~pos:6 ~len:(String.length secondary - 6)
            else secondary)
        |> List.filter ~f:(fun secondary ->
            if String.trim secondary = "" then false else true)
      in
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
      let range =
        Grace.Range.create ~source
          (Grace.Byte_index.of_lex pos_start)
          (Grace.Byte_index.of_lex pos_end)
      in
      let primary, notes = split_error_message message in
      let notes = List.map notes ~f:Grace.Diagnostic.Message.create in
      let primary_label =
        Grace.Diagnostic.Message.create primary
        |> Grace.Diagnostic.Label.primary ~range
      in
      let diagnostic =
        Grace.Diagnostic.createf Grace.Diagnostic.Severity.Error
          "Syntax error during verification" ~labels:[ primary_label ] ~notes
      in
      Error (`Syntax_error diagnostic)

let get_line_num (vc : Wp.vc) =
  let start, _ = vc.loc in
  start.pos_lnum

let check program =
  let vcs = Wp.verify program in
  let verdicts =
    List.map vcs ~f:(fun (vc : Wp.vc) ->
        (vc, Smt.Solver.check_alt_ergo vc.goal))
  in
  verdicts

let verdict_line ~num ~total (vc : Wp.vc) (verdict : Smt.Solver.smt_result) =
  let open Notty in
  let open Nottui in
  let icon, text, colour =
    match verdict with
    | Valid -> ("✓", "valid", A.green)
    | Invalid -> ("✗", "invalid", A.red)
    | Unknown _ -> ("?", "unknown", A.yellow)
  in
  let loc = Fmt.str "at line %d" (get_line_num vc) in
  Ui.hcat
    [
      Ui.atom (I.string A.(fg colour ++ st bold) icon);
      Ui.space 1 1;
      Ui.atom (I.string A.(fg colour) text);
      Ui.space 2 1;
      Ui.atom (I.strf "[%d/%d]" num total);
      Ui.space 2 1;
      Ui.resize ~w:30 (Nottui.Ui.atom (I.strf "%a" Wp.pp_kind vc.kind));
      Ui.resize ~w:20 ~sw:1
        ~pad:Gravity.(make ~h:`Negative ~v:`Negative)
        (Ui.atom (I.string A.empty loc));
    ]

let verdict_block ~num ~total ~show_goals (vc : Wp.vc)
    (verdict : Smt.Solver.smt_result) =
  let open Notty in
  let open Nottui in
  let main_line = verdict_line ~num ~total vc verdict in
  let goal_line =
    Ui.atom (I.strf ~attr:A.(fg lightblack) "    goal: %a" Formula.pp vc.goal)
  in
  match verdict with
  | Valid ->
      if not show_goals then main_line else Ui.vcat [ main_line; goal_line ]
  | Invalid | Unknown _ -> Ui.vcat [ main_line; goal_line ]

let parse_and_verify file show_goals =
  match parse_file file with
  | Ok (program, _content) ->
      let open Nottui in
      let open Notty in
      let verdicts = check program in
      let num_verdicts = List.length verdicts in
      let verdict_uis =
        List.mapi verdicts ~f:(fun i (vc, verdict) ->
            match verdict with
            | Ok verdict ->
                verdict_block ~num:(i + 1) ~total:num_verdicts ~show_goals vc
                  verdict
            | Error (`Msg message) ->
                Ui.atom (Notty.I.string A.(fg lightred ++ st italic) message))
      in
      let valid, invalid, unknown, error =
        List.fold_left verdicts ~init:(0, 0, 0, 0)
          ~f:(fun (valid, invalid, unknown, error) (_, verdict) ->
            match verdict with
            | Ok Smt.Solver.Valid -> (valid + 1, invalid, unknown, error)
            | Ok Invalid -> (valid, invalid + 1, unknown, error)
            | Ok (Unknown _) -> (valid, invalid, unknown + 1, error)
            | Error _ -> (valid, invalid, unknown, error + 1))
      in
      let cols =
        match Notty_unix.winsize Unix.stdout with
        | Some (c, _) -> c
        | None -> 80
      in
      let verdicts = Ui.vcat verdict_uis in
      let ui =
        Ui.vcat
          [
            Ui.atom (I.strf ~attr:A.(st bold) "Verifying %a" Fpath.pp file);
            Ui.atom (I.uchar A.empty (Uchar.of_int 0x2500) cols 1);
            verdicts;
            Ui.atom (I.uchar A.empty (Uchar.of_int 0x2500) cols 1);
            Ui.hcat
              [
                Ui.atom (I.strf ~attr:A.(fg green) "%d valid" valid);
                Ui.atom (I.string A.empty " · ");
                Ui.atom (I.strf ~attr:A.(fg red) "%d invalid" invalid);
                Ui.atom (I.string A.empty " · ");
                Ui.atom (I.strf ~attr:A.(fg yellow) "%d unknown" unknown);
                Ui.atom (I.string A.empty " · ");
                Ui.atom (I.strf ~attr:A.(fg lightred) "%d error" error);
              ];
          ]
      in
      let rows = Nottui.Ui.layout_height ui in
      let renderer = Renderer.make () in
      Renderer.update renderer (cols, rows) ui;
      Notty_unix.output_image (Renderer.image renderer);
      if invalid > 0 || unknown > 0 || error > 0 then Cmd.Exit.some_error
      else Cmd.Exit.ok
  | Error (`Syntax_error diagnostic) ->
      let diagnotic_pp =
        Grace_ansi_renderer.pp_diagnostic
          ~code_to_string:(fun _ -> "")
          ~config:Grace_ansi_renderer.Config.default
      in
      Fmt.epr "%a" diagnotic_pp diagnostic;
      Cmd.Exit.some_error
  | Error (`Msg message) ->
      Fmt.epr "liss-verify: %s" message;
      Cmd.Exit.some_error

let program_arg =
  let doc = "path to a liss program" in
  Arg.(required & pos 0 (some fpath) None & info [] ~docv:"FILE" ~doc)

let show_goals_arg =
  let doc = "show all goals" in
  Arg.(value & flag & info [ "g"; "goals" ] ~doc)

let cmd =
  let doc = "Verify a liss program" in
  let info = Cmd.info "liss-verify" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const parse_and_verify $ program_arg $ show_goals_arg)

let () = exit (Cmd.eval' cmd)
