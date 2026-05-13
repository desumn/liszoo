type t = { pre : Formula.t; body : Cmd.t; post : Formula.t }

let equal (prog1 : t) (prog2 : t) =
  Formula.(equal prog1.pre prog2.pre && equal prog1.post prog2.post)
  && Cmd.equal prog1.body prog2.body

let pp ppf prog =
  Fmt.pf ppf "@[<v>pre { %a }@,%a@,post { %a }@]" Formula.pp prog.pre Cmd.pp
    prog.body Formula.pp prog.post
