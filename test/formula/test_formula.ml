let set = Alcotest.(list string)
let term = Alcotest.testable Formula.Term.pp Formula.Term.equal
let atom = Alcotest.testable Formula.Atom.pp Formula.Atom.equal
let formula = Alcotest.testable Formula.pp Formula.equal

let fv_case name with_variable expected =
  Alcotest.test_case name `Quick @@ fun () ->
  match with_variable with
  | `Term term ->
      Alcotest.(
        check' set ~msg:name
          ~actual:(Common.StringSet.elements @@ Formula.Term.free_vars term)
          ~expected)
  | `Atom atom ->
      Alcotest.(
        check' set ~msg:name
          ~actual:(Common.StringSet.elements @@ Formula.Atom.free_vars atom)
          ~expected)
  | `Formula formula ->
      Alcotest.(
        check' set ~msg:name
          ~actual:(Common.StringSet.elements @@ Formula.free_vars formula)
          ~expected)

let subst_case name ~var ~by subst =
  Alcotest.test_case name `Quick @@ fun () ->
  match subst with
  | `Term (from, expected) ->
      Alcotest.(
        check' term ~msg:name
          ~actual:(Formula.Term.subst var ~by ~on:from)
          ~expected)
  | `Atom (from, expected) ->
      Alcotest.(
        check' atom ~msg:name ~actual:(Formula.Atom.subst var by from) ~expected)
  | `Formula (from, expected) ->
      Alcotest.(
        check' formula ~msg:name ~actual:(Formula.subst var by from) ~expected)

let pp_case name value expected =
  Alcotest.test_case name `Quick @@ fun () ->
  match value with
  | `Term term ->
      Alcotest.(
        check' string ~msg:name
          ~actual:(Fmt.str "%a" Formula.Term.pp term)
          ~expected)
  | `Atom atom ->
      Alcotest.(
        check' string ~msg:name
          ~actual:(Fmt.str "%a" Formula.Atom.pp atom)
          ~expected)
  | `Formula formula ->
      Alcotest.(
        check' string ~msg:name
          ~actual:(Fmt.str "%a" Formula.pp formula)
          ~expected)

let term_fv_tests =
  Formula.Term.
    [
      fv_case "fv single variable" (`Term (Var "x")) [ "x" ];
      fv_case "fv const" (`Term (Const 10)) [];
      fv_case "fv multiple variable"
        (`Term (Add (Var "x", Var "y")))
        [ "x"; "y" ];
      fv_case "fv deduplication"
        (`Term (Add (Mul (Var "x", Var "y"), Var "y")))
        [ "x"; "y" ];
      fv_case "fv composite" (`Term (Neg (Sub (Var "a", Var "b")))) [ "a"; "b" ];
    ]

let term_subst_tests =
  Formula.Term.
    [
      subst_case "simple subst" ~var:"x" ~by:(Const 43)
        (`Term (Var "x", Const 43));
      subst_case "subst on free variable" ~var:"x" ~by:(Const 43)
        (`Term (Var "y", Var "y"));
      subst_case "subst on no variable" ~var:"x" ~by:(Const 43)
        (`Term (Const 19, Const 19));
      subst_case "subst on unary" ~var:"x" ~by:(Const 43)
        (`Term (Neg (Var "x"), Neg (Const 43)));
      subst_case "subst on binary, multiple bound" ~var:"x" ~by:(Const 43)
        (`Term (Add (Var "x", Var "x"), Add (Const 43, Const 43)));
      subst_case "subst on complex" ~var:"x"
        ~by:(Add (Var "y", Const 1))
        (`Term
           ( Mul (Var "x", Var "x"),
             Mul (Add (Var "y", Const 1), Add (Var "y", Const 1)) ));
      subst_case "subst not fv" ~var:"z" ~by:(Const 43)
        (`Term (Add (Var "x", Var "y"), Add (Var "x", Var "y")));
    ]

let term_pp_tests =
  Formula.Term.
    [
      pp_case "pp variable" (`Term (Var "x")) "x";
      pp_case "pp constant" (`Term (Const 22)) "22";
      pp_case "pp negative constant" (`Term (Const (-7))) "-7";
      pp_case "pp negated variable" (`Term (Neg (Var "x"))) "-x";
      pp_case "pp double negated variable" (`Term (Neg (Neg (Var "x")))) "-(-x)";
      pp_case "pp add" (`Term (Add (Var "x", Const 10))) "x + 10";
      pp_case "pp sub, left assoc"
        (`Term (Sub (Sub (Var "a", Var "b"), Var "c")))
        "a - b - c";
      pp_case "pp sub, priority"
        (`Term (Sub (Var "a", Sub (Var "b", Var "c"))))
        "a - (b - c)";
      pp_case "pp operation, priority"
        (`Term (Mul (Add (Var "a", Var "b"), Var "c")))
        "(a + b) * c";
      pp_case "pp operation, mul priority"
        (`Term (Add (Mul (Var "a", Var "b"), Var "c")))
        "a * b + c";
      pp_case "pp negation, over operation"
        (`Term (Neg (Add (Var "a", Var "b"))))
        "-(a + b)";
      pp_case "pp negation, priority"
        (`Term (Add (Neg (Var "a"), Var "b")))
        "-a + b";
    ]

let atom_fv_tests =
  Formula.Atom.(
    Formula.Term.
      [
        fv_case "fv equal" (`Atom (Eq (Var "x", Const 10))) [ "x" ];
        fv_case "fv not equal" (`Atom (Neq (Var "x", Var "d"))) [ "d"; "x" ];
        fv_case "fv less than"
          (`Atom (Lt (Add (Var "a", Const 10), Var "d")))
          [ "a"; "d" ];
        fv_case "fv less equal" (`Atom (Le (Var "b", Var "d"))) [ "b"; "d" ];
        fv_case "fv greater than"
          (`Atom (Gt (Add (Var "a", Sub (Var "a", Var "b")), Var "d")))
          [ "a"; "b"; "d" ];
        fv_case "fv greater equal" (`Atom (Ge (Const 5, Const 10))) [];
      ])

let atom_subst_tests =
  Formula.Atom.(
    Formula.Term.
      [
        subst_case "subst in atom" ~var:"x" ~by:(Const 5)
          (`Atom
             ( Eq (Var "x", Add (Var "y", Var "x")),
               Eq (Const 5, Add (Var "y", Const 5)) ));
      ])

let atom_pp_tests =
  Formula.Atom.(
    Formula.Term.
      [
        pp_case "pp equal" (`Atom (Eq (Const 10, Const 11))) "10 = 11";
        pp_case "pp not equal" (`Atom (Neq (Var "x", Const 0))) "x <> 0";
        pp_case "pp less than" (`Atom (Lt (Var "y", Var "x"))) "y < x";
        pp_case "pp less equal" (`Atom (Le (Const 1, Var "y"))) "1 <= y";
        pp_case "pp greater than"
          (`Atom (Gt (Add (Var "x", Const 5), Const 10)))
          "x + 5 > 10";
        pp_case "pp greater than"
          (`Atom (Ge (Add (Var "x", Const 5), Sub (Var "y", Const 10))))
          "x + 5 >= y - 10";
      ])

let formula_fv_tests =
  Formula.(
    Atom.(
      Term.
        [
          fv_case "fv top" (`Formula Top) [];
          fv_case "fv bot" (`Formula Bot) [];
          fv_case "composite formula"
            (`Formula (Not (Atom (Eq (Var "x", Var "y")))))
            [ "x"; "y" ];
          fv_case "complexe"
            (`Formula
               (And
                  ( Atom (Eq (Var "x", Const 0)),
                    Imp (Atom (Lt (Var "y", Var "z")), Bot) )))
            [ "x"; "y"; "z" ];
        ]))

let formula_subst_tests =
  Formula.(
    Atom.(
      Term.
        [
          subst_case "subst on top" ~var:"x" ~by:(Const 5) (`Formula (Top, Top));
          subst_case "subst on nested" ~var:"x" ~by:(Const 10)
            (`Formula
               ( Not (Atom (Eq (Var "x", Const 0))),
                 Not (Atom (Eq (Const 10, Const 0))) ));
          subst_case "subst on complexe" ~var:"x" ~by:(Const 15)
            (`Formula
               ( And
                   ( Atom (Eq (Var "x", Var "y")),
                     Or (Atom (Lt (Var "x", Const 0)), Top) ),
                 And
                   ( Atom (Eq (Const 15, Var "y")),
                     Or (Atom (Lt (Const 15, Const 0)), Top) ) ));
        ]))

let formula_pp_tests =
  Formula.(
    Atom.(
      Term.
        [
          pp_case "pp top" (`Formula Top) "true";
          pp_case "pp bot" (`Formula Bot) "false";
          pp_case "pp atom eq" (`Formula (Atom (Eq (Var "x", Var "y")))) "x = y";
          pp_case "pp and" (`Formula (And (Top, Bot))) {|true /\ false|};
          pp_case "pp or" (`Formula (Or (Bot, Top))) {|false \/ true|};
          pp_case "pp imp" (`Formula (Imp (Bot, Top))) {|false => true|};
          pp_case "pp iff" (`Formula (Iff (Top, Top))) {|true <=> true|};
          pp_case "pp not atom"
            (`Formula (Not (Atom (Eq (Var "x", Var "y")))))
            "~(x = y)";
          pp_case "pp priority and over or"
            (`Formula (And (Or (Top, Bot), Bot)))
            {|(true \/ false) /\ false|};
          pp_case "pp priority and"
            (`Formula (Or (And (Top, Bot), Bot)))
            {|true /\ false \/ false|};
          pp_case "pp imp right assoc"
            (`Formula (Imp (Imp (Top, Top), Bot)))
            {|(true => true) => false|};
          pp_case "pp imp right assoc 2"
            (`Formula (Imp (Top, Imp (Top, Bot))))
            {|true => true => false|};
          pp_case "pp and left assoc"
            (`Formula (And (Top, And (Top, Bot))))
            {|true /\ (true /\ false)|};
          pp_case "pp and left assoc"
            (`Formula (Not (And (Top, Bot))))
            {|~(true /\ false)|};
        ]))

let () =
  let open Alcotest in
  run "formula"
    [
      ("term_fv", term_fv_tests);
      ("term_subst", term_subst_tests);
      ("term_pp", term_pp_tests);
      ("atom_fv", atom_fv_tests);
      ("atom_subst", atom_subst_tests);
      ("atom_pp", atom_pp_tests);
      ("formula_fv", formula_fv_tests);
      ("formula_subst", formula_subst_tests);
      ("formula_pp", formula_pp_tests);
    ]
