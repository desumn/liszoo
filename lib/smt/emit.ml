
let rec term ppf (in_term : Formula.Term.t) =
  match in_term with
  | Var x -> Fmt.string ppf x
  | Const n when n >= 0 -> Fmt.int ppf n
  | Const n -> Fmt.pf ppf "(- %d)" n
  | Neg t -> Fmt.pf ppf "(- %a)" term t
  | Add (l, r) -> Fmt.pf ppf "(+ %a %a)" term l term r
  | Sub (l, r) -> Fmt.pf ppf "(- %a %a)" term l term r
  | Mul (l, r) -> Fmt.pf ppf "(* %a %a)" term l term r

let atom ppf (in_atom : Formula.Atom.t) =
  match in_atom with
  | Eq (t1, t2) -> Fmt.pf ppf "(= %a %a)" term t1 term t2
  | Neq (t1, t2) -> Fmt.pf ppf "(distinct %a %a)" term t1 term t2
  | Lt (t1, t2) -> Fmt.pf ppf "(< %a %a)" term t1 term t2
  | Le (t1, t2) -> Fmt.pf ppf "(<= %a %a)" term t1 term t2
  | Gt (t1, t2) -> Fmt.pf ppf "(> %a %a)" term t1 term t2
  | Ge (t1, t2) -> Fmt.pf ppf "(>= %a %a)" term t1 term t2

let rec formula ppf (in_formula : Formula.t) = 
  match in_formula with
  | Top -> Fmt.string ppf "true"
  | Bot -> Fmt.string ppf "false"
  | Atom a -> Fmt.pf ppf "%a" atom a
  | Not f -> Fmt.pf ppf "(not %a)" formula f
  | And (l, r) -> Fmt.pf ppf "(and %a %a)" formula l formula r
  | Or (l, r) -> Fmt.pf ppf "(or %a %a)" formula l formula r
  | Imp (l, r) -> Fmt.pf ppf "(=> %a %a)" formula l formula r
  | Iff (l, r) -> Fmt.pf ppf "(= %a %a)" formula l formula r

let query ppf (in_query : Formula.t) =
  let vars = Formula.free_vars in_query in
  Fmt.string ppf "(set-logic QF_NIA)";
  Common.StringSet.iter (fun var -> Fmt.pf ppf "(declare-const %s Int)" var) vars;
  Fmt.pf ppf "(assert (not %a))" formula in_query;
  Fmt.string ppf "(check-sat)\n"
