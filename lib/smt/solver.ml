type smt_result = Valid | Invalid | Unknown of [ `Unknown | `No_result ]

let result_of_string = function
  | "sat" -> Invalid
  | "unsat" -> Valid
  | "unknown" -> Unknown `Unknown
  | _ -> Unknown `No_result

let match_result = Re.compile Re.(alt [ str "sat"; str "unsat"; str "unknown" ])

let check_alt_ergo query =
  let open Bos in
  let open Result.Syntax in
  let* file = OS.File.tmp "formula_%s" in
  let* _ = OS.File.writef file "%a" Emit.query query in
  let cmd = Cmd.(v "alt-ergo" % "" % "--input=smtlib2" % p file) in
  let+ output = OS.Cmd.to_string (OS.Cmd.run_out cmd) in
  let result =
    Option.map
      (fun g -> Re.Group.get g 0 |> result_of_string)
      (Re.exec_opt match_result output)
  in
  result |> Option.value ~default:(Unknown `No_result)
