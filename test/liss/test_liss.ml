open Liss

let set = Alcotest.(list string)
let expr_t = Alcotest.testable Expr.pp Expr.equal
let bexpr_t = Alcotest.testable Bool_expr.pp Bool_expr.equal
let cmd_t = Alcotest.testable Cmd.pp Cmd.equal
let prog_t = Alcotest.testable Program.pp Program.equal
let term_t = Alcotest.testable Formula.Term.pp Formula.Term.equal
let atom_t = Alcotest.testable Formula.Atom.pp Formula.Atom.equal
let form_t = Alcotest.testable Formula.pp Formula.equal

let mk = Common.Located.dummy

let with_loc line node =
  let p =
    Lexing.
      {
        pos_fname = "test";
        pos_lnum = line;
        pos_bol = 0;
        pos_cnum = line;
      }
  in
  Common.Located.make (p, p) node

(* === Expr === *)

let expr_fv_case name expr expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' set ~msg:name
      ~actual:(Common.StringSet.elements (Expr.free_vars expr))
      ~expected)

let expr_pp_case name expr expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' string ~msg:name ~actual:(Fmt.str "%a" Expr.pp expr) ~expected)

let expr_fv_tests =
  Expr.
    [
      expr_fv_case "fv variable" (Var "x") [ "x" ];
      expr_fv_case "fv const" (Const 5) [];
      expr_fv_case "fv negation" (Neg (Var "x")) [ "x" ];
      expr_fv_case "fv add" (Add (Var "x", Var "y")) [ "x"; "y" ];
      expr_fv_case "fv dedup"
        (Add (Var "x", Mul (Var "x", Var "y")))
        [ "x"; "y" ];
      expr_fv_case "fv composite"
        (Sub (Mul (Var "a", Var "b"), Neg (Var "c")))
        [ "a"; "b"; "c" ];
    ]

let expr_pp_tests =
  Expr.
    [
      expr_pp_case "pp var" (Var "x") "x";
      expr_pp_case "pp const positive" (Const 22) "22";
      expr_pp_case "pp const negative" (Const (-7)) "-7";
      expr_pp_case "pp negation" (Neg (Var "x")) "-x";
      expr_pp_case "pp double negation" (Neg (Neg (Var "x"))) "-(-x)";
      expr_pp_case "pp add" (Add (Var "x", Const 10)) "x + 10";
      expr_pp_case "pp sub left assoc"
        (Sub (Sub (Var "a", Var "b"), Var "c"))
        "a - b - c";
      expr_pp_case "pp sub right assoc"
        (Sub (Var "a", Sub (Var "b", Var "c")))
        "a - (b - c)";
      expr_pp_case "pp mul priority"
        (Mul (Add (Var "a", Var "b"), Var "c"))
        "(a + b) * c";
      expr_pp_case "pp add over mul"
        (Add (Mul (Var "a", Var "b"), Var "c"))
        "a * b + c";
      expr_pp_case "pp neg over add"
        (Neg (Add (Var "a", Var "b")))
        "-(a + b)";
    ]

let expr_equal_tests =
  Expr.
    [
      Alcotest.test_case "equal same" `Quick (fun () ->
          Alcotest.(
            check' expr_t ~msg:"same var" ~actual:(Var "x") ~expected:(Var "x")));
      Alcotest.test_case "not equal different var" `Quick (fun () ->
          Alcotest.(check bool)
            "different vars" false
            (equal (Var "x") (Var "y")));
      Alcotest.test_case "not equal different shape" `Quick (fun () ->
          Alcotest.(check bool)
            "var vs const" false
            (equal (Var "x") (Const 0)));
      Alcotest.test_case "compare same is zero" `Quick (fun () ->
          Alcotest.(check int)
            "compare same" 0
            (compare (Var "x") (Var "x")));
      Alcotest.test_case "compare different is nonzero" `Quick (fun () ->
          Alcotest.(check bool)
            "compare different" true
            (compare (Var "a") (Var "b") <> 0));
    ]

(* === Bool_expr === *)

let bexpr_fv_case name b expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' set ~msg:name
      ~actual:(Common.StringSet.elements (Bool_expr.free_vars b))
      ~expected)

let bexpr_pp_case name b expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' string ~msg:name ~actual:(Fmt.str "%a" Bool_expr.pp b) ~expected)

let bexpr_atom_pp_case name a expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' string ~msg:name
      ~actual:(Fmt.str "%a" Bool_expr.pp_atom a)
      ~expected)

let bexpr_atom_pp_tests =
  Bool_expr.(
    Expr.
      [
        bexpr_atom_pp_case "atom eq" (Eq (Var "x", Const 0)) "x = 0";
        bexpr_atom_pp_case "atom neq" (Neq (Var "x", Const 0)) "x /= 0";
        bexpr_atom_pp_case "atom lt" (Lt (Var "x", Var "y")) "x < y";
        bexpr_atom_pp_case "atom le" (Le (Const 1, Var "y")) "1 <= y";
        bexpr_atom_pp_case "atom gt" (Gt (Var "x", Const 0)) "x > 0";
        bexpr_atom_pp_case "atom ge" (Ge (Var "x", Const 0)) "x >= 0";
        bexpr_atom_pp_case "atom with expressions"
          (Eq (Add (Var "x", Const 1), Mul (Var "y", Const 2)))
          "x + 1 = y * 2";
      ])

let bexpr_fv_tests =
  Bool_expr.(
    Expr.
      [
        bexpr_fv_case "fv true" True [];
        bexpr_fv_case "fv false" False [];
        bexpr_fv_case "fv atom" (Atom (Eq (Var "x", Const 0))) [ "x" ];
        bexpr_fv_case "fv not" (Not (Atom (Eq (Var "x", Const 0)))) [ "x" ];
        bexpr_fv_case "fv and"
          (And (Atom (Eq (Var "x", Const 0)), Atom (Lt (Var "y", Const 10))))
          [ "x"; "y" ];
        bexpr_fv_case "fv or dedup"
          (Or (Atom (Eq (Var "x", Var "y")), Atom (Lt (Var "x", Const 0))))
          [ "x"; "y" ];
        bexpr_fv_case "fv composite"
          (Or
             ( Not (Atom (Eq (Var "x", Var "y"))),
               And
                 ( Atom (Gt (Var "z", Const 0)),
                   Atom (Lt (Var "x", Const 100)) ) ))
          [ "x"; "y"; "z" ];
      ])

let bexpr_pp_tests =
  Bool_expr.(
    Expr.
      [
        bexpr_pp_case "pp true" True "true";
        bexpr_pp_case "pp false" False "false";
        bexpr_pp_case "pp atom" (Atom (Eq (Var "x", Const 0))) "x = 0";
        bexpr_pp_case "pp not atom"
          (Not (Atom (Eq (Var "x", Const 0))))
          "not (x = 0)";
        bexpr_pp_case "pp not non-atom" (Not True) "not true";
        bexpr_pp_case "pp double not"
          (Not (Not (Atom (Eq (Var "x", Const 0)))))
          "not not (x = 0)";
        bexpr_pp_case "pp and" (And (True, False)) "true and false";
        bexpr_pp_case "pp or" (Or (True, False)) "true or false";
        bexpr_pp_case "pp and over or, parens"
          (And (Or (True, False), True))
          "(true or false) and true";
        bexpr_pp_case "pp and binds tighter than or"
          (Or (And (True, False), True))
          "true and false or true";
        bexpr_pp_case "pp not over and"
          (Not (And (True, False)))
          "not (true and false)";
        bexpr_pp_case "pp and right assoc parens"
          (And (True, And (True, False)))
          "true and (true and false)";
      ])

let bexpr_equal_tests =
  Bool_expr.(
    Expr.
      [
        Alcotest.test_case "equal true" `Quick (fun () ->
            Alcotest.(
              check' bexpr_t ~msg:"true = true" ~actual:True ~expected:True));
        Alcotest.test_case "not equal true vs false" `Quick (fun () ->
            Alcotest.(check bool) "different" false (Bool_expr.equal True False));
        Alcotest.test_case "equal nested" `Quick (fun () ->
            let l = And (Atom (Eq (Var "x", Const 0)), True) in
            let r = And (Atom (Eq (Var "x", Const 0)), True) in
            Alcotest.(check' bexpr_t ~msg:"nested equal" ~actual:l ~expected:r));
      ])

(* === Cmd === *)

let cmd_pp_case name c expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(
    check' string ~msg:name ~actual:(Fmt.str "%a" Cmd.pp c) ~expected)

(* small DSL helpers to keep test cases readable *)
let skip = mk Cmd.Skip
let assign x e = mk (Cmd.Assign (x, e))
let seq a b = mk (Cmd.Seq (a, b))
let if_ c t e = mk (Cmd.If (c, t, e))
let while_ c i b = mk (Cmd.While (c, i, b))
let assert_ f = mk (Cmd.Assert f)
let assume_ f = mk (Cmd.Assume f)
let x_eq_0 = Bool_expr.(Atom (Eq (Expr.Var "x", Expr.Const 0)))

let cmd_pp_tests =
  [
    cmd_pp_case "pp skip" skip "skip";
    cmd_pp_case "pp assign" (assign "x" (Expr.Const 0)) "x := 0";
    cmd_pp_case "pp assign expr"
      (assign "x" Expr.(Add (Var "y", Const 1)))
      "x := y + 1";
    cmd_pp_case "pp assert" (assert_ Formula.Top) "assert { true }";
    cmd_pp_case "pp assume" (assume_ Formula.Bot) "assume { false }";
    cmd_pp_case "pp seq"
      (seq (assign "x" (Expr.Const 0)) (assign "y" (Expr.Const 1)))
      "x := 0;\ny := 1";
    cmd_pp_case "pp seq triple"
      (seq
         (assign "x" (Expr.Const 0))
         (seq (assign "y" (Expr.Const 1)) (assign "z" (Expr.Const 2))))
      "x := 0;\ny := 1;\nz := 2";
    cmd_pp_case "pp if"
      (if_ x_eq_0 skip skip)
      "if x = 0 then\n  skip\nelse\n  skip\nend";
    cmd_pp_case "pp while"
      (while_ x_eq_0 Formula.Top skip)
      "while x = 0 invariant { true } do\n  skip\nend";
    cmd_pp_case "pp if with seq body"
      (if_ x_eq_0
         (seq (assign "x" (Expr.Const 1)) (assign "y" (Expr.Const 2)))
         skip)
      "if x = 0 then\n  x := 1;\n  y := 2\nelse\n  skip\nend";
    cmd_pp_case "pp while with seq body"
      (while_ x_eq_0 Formula.Top
         (seq
            (assign "x" Expr.(Add (Var "x", Const 1)))
            (assign "y" Expr.(Sub (Var "y", Const 1)))))
      "while x = 0 invariant { true } do\n  x := x + 1;\n  y := y - 1\nend";
    cmd_pp_case "pp nested if"
      (if_ x_eq_0 (if_ Bool_expr.True skip skip) skip)
      "if x = 0 then\n  if true then\n    skip\n  else\n    skip\n  \
       end\nelse\n  skip\nend";
    cmd_pp_case "pp seq with if"
      (seq (if_ x_eq_0 skip skip) (assign "x" (Expr.Const 1)))
      "if x = 0 then\n  skip\nelse\n  skip\nend;\nx := 1";
  ]

let cmd_equal_tests =
  [
    Alcotest.test_case "equal: skip with different locs" `Quick (fun () ->
        let a = with_loc 1 Cmd.Skip in
        let b = with_loc 99 Cmd.Skip in
        Alcotest.(check' cmd_t ~msg:"skip ignoring loc" ~actual:a ~expected:b));
    Alcotest.test_case "equal: nested locs ignored" `Quick (fun () ->
        let a =
          with_loc 1
            (Cmd.Seq
               ( with_loc 2 (Cmd.Assign ("x", Expr.Const 0)),
                 with_loc 3 Cmd.Skip ))
        in
        let b =
          with_loc 100
            (Cmd.Seq
               ( with_loc 200 (Cmd.Assign ("x", Expr.Const 0)),
                 with_loc 300 Cmd.Skip ))
        in
        Alcotest.(check' cmd_t ~msg:"deep equal" ~actual:a ~expected:b));
    Alcotest.test_case "not equal: assign different var" `Quick (fun () ->
        Alcotest.(check bool)
          "different var" false
          (Cmd.equal
             (assign "x" (Expr.Const 0))
             (assign "y" (Expr.Const 0))));
    Alcotest.test_case "not equal: assign different rhs" `Quick (fun () ->
        Alcotest.(check bool)
          "different rhs" false
          (Cmd.equal
             (assign "x" (Expr.Const 0))
             (assign "x" (Expr.Const 1))));
    Alcotest.test_case "not equal: different node" `Quick (fun () ->
        Alcotest.(check bool)
          "different node" false
          (Cmd.equal skip (assign "x" (Expr.Const 0))));
    Alcotest.test_case "not equal: if branches swapped" `Quick (fun () ->
        Alcotest.(check bool)
          "different then/else" false
          (Cmd.equal
             (if_ x_eq_0 skip (assign "x" (Expr.Const 1)))
             (if_ x_eq_0 (assign "x" (Expr.Const 1)) skip)));
  ]

(* === Program === *)

let prog_pp_tests =
  [
    Alcotest.test_case "pp trivial program" `Quick (fun () ->
        let prog : Program.t =
          { pre = Formula.Top; body = skip; post = Formula.Top }
        in
        Alcotest.(
          check' string ~msg:"trivial"
            ~actual:(Fmt.str "%a" Program.pp prog)
            ~expected:"pre { true }\nskip\npost { true }"));
    Alcotest.test_case "pp multi-stmt program" `Quick (fun () ->
        let prog : Program.t =
          {
            pre = Formula.(Atom (Atom.Eq (Term.Var "n", Term.Const 0)));
            body =
              seq (assign "x" (Expr.Const 0)) (assign "y" (Expr.Const 1));
            post = Formula.(Atom (Atom.Eq (Term.Var "x", Term.Var "y")));
          }
        in
        Alcotest.(
          check' string ~msg:"multi-stmt"
            ~actual:(Fmt.str "%a" Program.pp prog)
            ~expected:"pre { n = 0 }\nx := 0;\ny := 1\npost { x = y }"));
    Alcotest.test_case "pp program with loop" `Quick (fun () ->
        let prog : Program.t =
          {
            pre = Formula.(Atom (Atom.Ge (Term.Var "n", Term.Const 0)));
            body =
              seq
                (assign "i" (Expr.Const 0))
                (while_
                   Bool_expr.(Atom (Lt (Expr.Var "i", Expr.Var "n")))
                   Formula.Top
                   (assign "i" Expr.(Add (Var "i", Const 1))));
            post = Formula.(Atom (Atom.Eq (Term.Var "i", Term.Var "n")));
          }
        in
        Alcotest.(
          check' string ~msg:"loop program"
            ~actual:(Fmt.str "%a" Program.pp prog)
            ~expected:
              "pre { n >= 0 }\n\
               i := 0;\n\
               while i < n invariant { true } do\n\
              \  i := i + 1\n\
               end\n\
               post { i = n }"));
  ]

let prog_equal_tests =
  [
    Alcotest.test_case "equal: identical programs" `Quick (fun () ->
        let p : Program.t =
          { pre = Formula.Top; body = skip; post = Formula.Top }
        in
        let q : Program.t =
          { pre = Formula.Top; body = skip; post = Formula.Top }
        in
        Alcotest.(check' prog_t ~msg:"identical" ~actual:p ~expected:q));
    Alcotest.test_case "not equal: different pre" `Quick (fun () ->
        let p : Program.t =
          { pre = Formula.Top; body = skip; post = Formula.Top }
        in
        let q : Program.t =
          { pre = Formula.Bot; body = skip; post = Formula.Top }
        in
        Alcotest.(check bool) "different pre" false (Program.equal p q));
    Alcotest.test_case "equal: ignores body locs" `Quick (fun () ->
        let p : Program.t =
          {
            pre = Formula.Top;
            body = with_loc 1 Cmd.Skip;
            post = Formula.Top;
          }
        in
        let q : Program.t =
          {
            pre = Formula.Top;
            body = with_loc 99 Cmd.Skip;
            post = Formula.Top;
          }
        in
        Alcotest.(
          check' prog_t ~msg:"equal despite different body loc" ~actual:p
            ~expected:q));
  ]

(* === Conv === *)

let conv_to_term_case name expr expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check' term_t ~msg:name ~actual:(Conv.to_term expr) ~expected)

let conv_to_atom_case name a expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check' atom_t ~msg:name ~actual:(Conv.to_atom a) ~expected)

let conv_to_formula_case name b expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check' form_t ~msg:name ~actual:(Conv.to_formula b) ~expected)

let conv_to_term_tests =
  [
    conv_to_term_case "var" (Expr.Var "x") (Formula.Term.Var "x");
    conv_to_term_case "const" (Expr.Const 42) (Formula.Term.Const 42);
    conv_to_term_case "neg"
      (Expr.Neg (Expr.Var "x"))
      (Formula.Term.Neg (Formula.Term.Var "x"));
    conv_to_term_case "add"
      (Expr.Add (Expr.Var "x", Expr.Const 1))
      Formula.Term.(Add (Var "x", Const 1));
    conv_to_term_case "sub"
      (Expr.Sub (Expr.Var "x", Expr.Const 1))
      Formula.Term.(Sub (Var "x", Const 1));
    conv_to_term_case "mul"
      (Expr.Mul (Expr.Var "x", Expr.Const 2))
      Formula.Term.(Mul (Var "x", Const 2));
    conv_to_term_case "composite"
      Expr.(Mul (Add (Var "a", Const 1), Sub (Var "b", Var "c")))
      Formula.Term.(Mul (Add (Var "a", Const 1), Sub (Var "b", Var "c")));
  ]

let conv_to_atom_tests =
  Bool_expr.(
    Expr.
      [
        conv_to_atom_case "eq" (Eq (Var "x", Const 0))
          Formula.(Atom.Eq (Term.Var "x", Term.Const 0));
        conv_to_atom_case "neq" (Neq (Var "x", Const 0))
          Formula.(Atom.Neq (Term.Var "x", Term.Const 0));
        conv_to_atom_case "lt" (Lt (Var "x", Var "y"))
          Formula.(Atom.Lt (Term.Var "x", Term.Var "y"));
        conv_to_atom_case "le" (Le (Const 1, Var "y"))
          Formula.(Atom.Le (Term.Const 1, Term.Var "y"));
        conv_to_atom_case "gt" (Gt (Var "x", Const 0))
          Formula.(Atom.Gt (Term.Var "x", Term.Const 0));
        conv_to_atom_case "ge" (Ge (Var "x", Const 0))
          Formula.(Atom.Ge (Term.Var "x", Term.Const 0));
      ])

let conv_to_formula_tests =
  Bool_expr.(
    Expr.
      [
        conv_to_formula_case "true" True Formula.Top;
        conv_to_formula_case "false" False Formula.Bot;
        conv_to_formula_case "atom"
          (Atom (Eq (Var "x", Const 0)))
          Formula.(Atom Atom.(Eq (Term.Var "x", Term.Const 0)));
        conv_to_formula_case "not" (Not True) Formula.(Not Top);
        conv_to_formula_case "and" (And (True, False)) Formula.(And (Top, Bot));
        conv_to_formula_case "or" (Or (True, False)) Formula.(Or (Top, Bot));
        conv_to_formula_case "composite"
          (And
             ( Atom (Eq (Var "x", Const 0)),
               Not (Atom (Lt (Var "y", Const 10))) ))
          Formula.(
            And
              ( Atom Atom.(Eq (Term.Var "x", Term.Const 0)),
                Not (Atom Atom.(Lt (Term.Var "y", Term.Const 10))) ));
      ])

(* === Located === *)

let located_tests =
  [
    Alcotest.test_case "make sets node" `Quick (fun () ->
        let r =
          Common.Located.make (Lexing.dummy_pos, Lexing.dummy_pos) 42
        in
        Alcotest.(check int) "node" 42 r.node);
    Alcotest.test_case "map transforms node only" `Quick (fun () ->
        let r =
          Common.Located.make (Lexing.dummy_pos, Lexing.dummy_pos) 42
        in
        let r' = Common.Located.map (fun x -> x + 1) r in
        Alcotest.(check int) "transformed node" 43 r'.node);
    Alcotest.test_case "dummy yields lexing dummy positions" `Quick (fun () ->
        let r = Common.Located.dummy "abc" in
        Alcotest.(check string) "node" "abc" r.node;
        Alcotest.(check string)
          "fname" Lexing.dummy_pos.pos_fname (fst r.loc).pos_fname);
  ]

(* === Parser === *)

let parse = Liss.parse_string

let triv body : Program.t =
  { pre = Formula.Top; body; post = Formula.Top }

(* atom helpers : variable + constante entière *)
let b_eq x n = Bool_expr.(Atom (Eq (Expr.Var x, Expr.Const n)))
let b_lt x n = Bool_expr.(Atom (Lt (Expr.Var x, Expr.Const n)))
let b_gt x n = Bool_expr.(Atom (Gt (Expr.Var x, Expr.Const n)))

let f_eq x n = Formula.(Atom (Atom.Eq (Term.Var x, Term.Const n)))
let f_ge x n = Formula.(Atom (Atom.Ge (Term.Var x, Term.Const n)))

let parse_case name source expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check' prog_t ~msg:name ~actual:(parse source) ~expected)

(* Source → AST : un cas par constructeur *)
let parser_basic_tests =
  [
    parse_case "parse skip"
      "pre { true } skip post { true }"
      (triv skip);
    parse_case "parse assign const"
      "pre { true } x := 0 post { true }"
      (triv (assign "x" (Expr.Const 0)));
    parse_case "parse assign var"
      "pre { true } x := y post { true }"
      (triv (assign "x" (Expr.Var "y")));
    parse_case "parse seq"
      "pre { true } x := 0; y := 1 post { true }"
      (triv (seq (assign "x" (Expr.Const 0)) (assign "y" (Expr.Const 1))));
    parse_case "parse if"
      "pre { true } if x = 0 then skip else skip end post { true }"
      (triv (if_ (b_eq "x" 0) skip skip));
    parse_case "parse while"
      "pre { true } while x > 0 invariant { true } do skip end post { true }"
      (triv (while_ (b_gt "x" 0) Formula.Top skip));
    parse_case "parse assert"
      "pre { true } assert { x = 0 } post { true }"
      (triv (assert_ (f_eq "x" 0)));
    parse_case "parse assume"
      "pre { true } assume { false } post { true }"
      (triv (assume_ Formula.Bot));
    parse_case "parse non-trivial pre/post"
      "pre { x >= 0 } skip post { x = 0 }"
      { pre = f_ge "x" 0; body = skip; post = f_eq "x" 0 };
  ]

(* Précédences et associativités *)
let parser_precedence_tests =
  [
    parse_case "expr: mul over add"
      "pre { true } x := a + b * c post { true }"
      (triv (assign "x" Expr.(Add (Var "a", Mul (Var "b", Var "c")))));
    parse_case "expr: parens override"
      "pre { true } x := (a + b) * c post { true }"
      (triv (assign "x" Expr.(Mul (Add (Var "a", Var "b"), Var "c"))));
    parse_case "expr: unary minus tighter than mul"
      "pre { true } x := -a * b post { true }"
      (triv (assign "x" Expr.(Mul (Neg (Var "a"), Var "b"))));
    parse_case "expr: double unary"
      "pre { true } x := - -a post { true }"
      (triv (assign "x" Expr.(Neg (Neg (Var "a")))));
    parse_case "expr: sub left-assoc"
      "pre { true } x := a - b - c post { true }"
      (triv (assign "x" Expr.(Sub (Sub (Var "a", Var "b"), Var "c"))));
    parse_case "bexpr: and binds tighter than or"
      "pre { true } if a = 0 and b = 0 or c = 0 then skip else skip end post \
       { true }"
      (triv
         (if_
            Bool_expr.(
              Or
                ( And (Atom (Eq (Expr.Var "a", Expr.Const 0)),
                       Atom (Eq (Expr.Var "b", Expr.Const 0))),
                  Atom (Eq (Expr.Var "c", Expr.Const 0)) ))
            skip skip));
    parse_case "bexpr: not binds tighter than and"
      "pre { true } if not x = 0 and y = 0 then skip else skip end post { true }"
      (triv
         (if_
            Bool_expr.(
              And
                ( Not (Atom (Eq (Expr.Var "x", Expr.Const 0))),
                  Atom (Eq (Expr.Var "y", Expr.Const 0)) ))
            skip skip));
    parse_case "formula: /\\ binds tighter than \\/"
      "pre { x = 0 \\/ y = 0 /\\ z = 0 } skip post { true }"
      {
        pre =
          Formula.(
            Or
              ( Atom (Atom.Eq (Term.Var "x", Term.Const 0)),
                And
                  ( Atom (Atom.Eq (Term.Var "y", Term.Const 0)),
                    Atom (Atom.Eq (Term.Var "z", Term.Const 0)) ) ));
        body = skip;
        post = Formula.Top;
      };
    parse_case "formula: => right-assoc"
      "pre { x = 0 => y = 0 => z = 0 } skip post { true }"
      {
        pre =
          Formula.(
            Imp
              ( Atom (Atom.Eq (Term.Var "x", Term.Const 0)),
                Imp
                  ( Atom (Atom.Eq (Term.Var "y", Term.Const 0)),
                    Atom (Atom.Eq (Term.Var "z", Term.Const 0)) ) ));
        body = skip;
        post = Formula.Top;
      };
    parse_case "formula: ~ binds tighter than /\\"
      "pre { ~(x = 0) /\\ y = 0 } skip post { true }"
      {
        pre =
          Formula.(
            And
              ( Not (Atom (Atom.Eq (Term.Var "x", Term.Const 0))),
                Atom (Atom.Eq (Term.Var "y", Term.Const 0)) ));
        body = skip;
        post = Formula.Top;
      };
  ]

(* Spécificités du lexer *)
let parser_lex_tests =
  [
    parse_case "leading comment"
      "(* before *) pre { true } skip post { true }"
      (triv skip);
    parse_case "comment inside annotation"
      "pre { true (* inside *) } skip post { true }"
      (triv skip);
    parse_case "nested comments"
      "pre { true } (* outer (* inner *) more *) skip post { true }"
      (triv skip);
    parse_case "underscore in integer"
      "pre { x = 1_000_000 } skip post { true }"
      {
        pre =
          Formula.(Atom (Atom.Eq (Term.Var "x", Term.Const 1_000_000)));
        body = skip;
        post = Formula.Top;
      };
    parse_case "multi-line program"
      "pre { true }\nx := 0;\ny := 1\npost { true }"
      (triv (seq (assign "x" (Expr.Const 0)) (assign "y" (Expr.Const 1))));
    parse_case "multi-line annotation"
      "pre {\n  x = 0\n  /\\\n  y = 1\n}\nskip\npost { true }"
      {
        pre =
          Formula.(
            And
              ( Atom (Atom.Eq (Term.Var "x", Term.Const 0)),
                Atom (Atom.Eq (Term.Var "y", Term.Const 1)) ));
        body = skip;
        post = Formula.Top;
      };
  ]

(* Round-trip : pp puis parse doit redonner le même AST *)
let round_trip_case name prog =
  Alcotest.test_case name `Quick @@ fun () ->
  let s = Fmt.str "%a" Program.pp prog in
  let parsed = parse s in
  Alcotest.(check' prog_t ~msg:name ~actual:parsed ~expected:prog)

let parser_round_trip_tests =
  [
    round_trip_case "trivial" (triv skip);
    round_trip_case "assign const" (triv (assign "x" (Expr.Const 42)));
    round_trip_case "assign expr"
      (triv (assign "x" Expr.(Add (Var "y", Const 1))));
    round_trip_case "seq"
      (triv (seq (assign "x" (Expr.Const 0)) (assign "y" (Expr.Const 1))));
    round_trip_case "seq triple"
      (triv
         (seq
            (assign "x" (Expr.Const 0))
            (seq (assign "y" (Expr.Const 1)) (assign "z" (Expr.Const 2)))));
    round_trip_case "if-else" (triv (if_ (b_eq "x" 0) skip skip));
    round_trip_case "while"
      (triv (while_ (b_gt "x" 0) Formula.Top skip));
    round_trip_case "assert" (triv (assert_ (f_eq "x" 0)));
    round_trip_case "assume" (triv (assume_ Formula.Bot));
    round_trip_case "nested if"
      (triv (if_ (b_eq "x" 0) (if_ Bool_expr.True skip skip) skip));
    round_trip_case "if with seq body"
      (triv
         (if_ (b_eq "x" 0)
            (seq (assign "x" (Expr.Const 1)) (assign "y" (Expr.Const 2)))
            skip));
    round_trip_case "arithmetic with parens"
      (triv
         (assign "x"
            Expr.(
              Mul (Add (Var "a", Var "b"), Sub (Var "c", Const 5)))));
    round_trip_case "complex bexpr"
      (triv
         (if_
            Bool_expr.(
              And
                ( Or
                    ( Atom (Eq (Expr.Var "x", Expr.Const 0)),
                      Atom (Lt (Expr.Var "y", Expr.Const 10)) ),
                  Not (Atom (Gt (Expr.Var "z", Expr.Const 0))) ))
            skip skip));
    round_trip_case "formula with imp/iff"
      {
        pre =
          Formula.(
            Imp
              ( Atom (Atom.Eq (Term.Var "x", Term.Const 0)),
                Iff
                  ( Atom (Atom.Eq (Term.Var "y", Term.Const 0)),
                    Bot ) ));
        body = skip;
        post = Formula.Top;
      };
    round_trip_case "formula with negation"
      {
        pre = Formula.(Not (Atom (Atom.Eq (Term.Var "x", Term.Const 0))));
        body = skip;
        post = Formula.Top;
      };
    round_trip_case "full program with loop"
      {
        pre = f_ge "n" 0;
        body =
          seq
            (assign "i" (Expr.Const 0))
            (while_
               Bool_expr.(Atom (Lt (Expr.Var "i", Expr.Var "n")))
               Formula.(
                 And
                   ( Atom (Atom.Le (Term.Var "i", Term.Var "n")),
                     Atom (Atom.Ge (Term.Var "i", Term.Const 0)) ))
               (assign "i" Expr.(Add (Var "i", Const 1))));
        post =
          Formula.(Atom (Atom.Eq (Term.Var "i", Term.Var "n")));
      };
  ]

let () =
  let open Alcotest in
  run "liss"
    [
      ("expr_fv", expr_fv_tests);
      ("expr_pp", expr_pp_tests);
      ("expr_equal", expr_equal_tests);
      ("bool_expr_atom_pp", bexpr_atom_pp_tests);
      ("bool_expr_fv", bexpr_fv_tests);
      ("bool_expr_pp", bexpr_pp_tests);
      ("bool_expr_equal", bexpr_equal_tests);
      ("cmd_pp", cmd_pp_tests);
      ("cmd_equal", cmd_equal_tests);
      ("program_pp", prog_pp_tests);
      ("program_equal", prog_equal_tests);
      ("conv_to_term", conv_to_term_tests);
      ("conv_to_atom", conv_to_atom_tests);
      ("conv_to_formula", conv_to_formula_tests);
      ("located", located_tests);
      ("parser_basic", parser_basic_tests);
      ("parser_precedence", parser_precedence_tests);
      ("parser_lex", parser_lex_tests);
      ("parser_round_trip", parser_round_trip_tests);
    ]
