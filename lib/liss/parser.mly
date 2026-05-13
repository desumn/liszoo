%{
%}
(* ==== Tokens ==== *)
%token <int> INT
%token <string> IDENT
%token SKIP IF THEN ELSE END WHILE INVARIANT DO
%token ASSERT ASSUME PRE POST
%token NOT AND OR TRUE FALSE
%token TILDE WEDGE VEE IMP IFF
%token PLUS MINUS STAR ASSIGN
%token EQ NEQ_PROG NEQ_LOG LT LE GT GE
%token LPAREN RPAREN LBRACE RBRACE SEMI
%token UMINUS  
%token EOF


%right SEMI
%right IFF
%right IMP
%left  VEE
%left  WEDGE
%right TILDE
%left  OR
%left  AND
%right NOT

%left  PLUS MINUS
%left  STAR
%right UMINUS


%start <Program.t> program


%%

let parens(X) ==
  | LPAREN; x = X; RPAREN;   { x }
let braced(X) ==
  | LBRACE; x = X; RBRACE;   { x }
let located(X) ==
  | x = X;  { Common.Located.make $loc x }

let program :=
  | PRE; pre = braced(formula);
    body = cmd;
    POST; post = braced(formula);
    EOF;
    { Program.{ pre; body; post } }

let cmd :=
  | l = cmd; SEMI; r = cmd;
      { Common.Located.make $loc (Cmd.Seq (l, r)) }
  | c = located(cmd_node);
      { c }

let cmd_node :=
  | SKIP;
      { Cmd.Skip }
  | id = IDENT; ASSIGN; e = expr;
      { Cmd.Assign (id, e) }
  | IF; c = bexpr; THEN; t = cmd; ELSE; e = cmd; END;
      { Cmd.If (c, t, e) }
  | WHILE; c = bexpr; INVARIANT; i = braced(formula); DO; b = cmd; END;
      { Cmd.While (c, i, b) }
  | ASSERT; f = braced(formula);
      { Cmd.Assert f }
  | ASSUME; f = braced(formula);
      { Cmd.Assume f }

let expr :=
  | l = expr; PLUS;  r = expr;            <Expr.Add>
  | l = expr; MINUS; r = expr;            <Expr.Sub>
  | l = expr; STAR;  r = expr;            <Expr.Mul>
  | MINUS; e = expr;   %prec UMINUS       <Expr.Neg>
  | n = INT;                              <Expr.Const>
  | x = IDENT;                            <Expr.Var>
  | e = parens(expr);                     { e }

let bexpr :=
  | l = bexpr; OR;  r = bexpr;            <Bool_expr.Or>
  | l = bexpr; AND; r = bexpr;            <Bool_expr.And>
  | NOT; b = bexpr;                       <Bool_expr.Not>
  | TRUE;                                 { Bool_expr.True }
  | FALSE;                                { Bool_expr.False }
  | b = parens(bexpr);                    { b }
  | l = expr; op = cmp_prog; r = expr;    { Bool_expr.Atom (op l r) }

let cmp_prog ==
  | EQ;        { fun l r -> Bool_expr.Eq  (l, r) }
  | NEQ_PROG;  { fun l r -> Bool_expr.Neq (l, r) }
  | LT;        { fun l r -> Bool_expr.Lt  (l, r) }
  | LE;        { fun l r -> Bool_expr.Le  (l, r) }
  | GT;        { fun l r -> Bool_expr.Gt  (l, r) }
  | GE;        { fun l r -> Bool_expr.Ge  (l, r) }

let formula :=
  | l = formula; IFF;   r = formula;      <Formula.Iff>
  | l = formula; IMP;   r = formula;      <Formula.Imp>
  | l = formula; VEE;   r = formula;      <Formula.Or>
  | l = formula; WEDGE; r = formula;      <Formula.And>
  | TILDE; f = formula;                   <Formula.Not>
  | TRUE;                                 { Formula.Top }
  | FALSE;                                { Formula.Bot }
  | f = parens(formula);                  { f }
  | l = term; op = cmp_log; r = term;     { Formula.Atom (op l r) }

let cmp_log ==
  | EQ;       { fun l r -> Formula.Atom.Eq  (l, r) }
  | NEQ_LOG;  { fun l r -> Formula.Atom.Neq (l, r) }
  | LT;       { fun l r -> Formula.Atom.Lt  (l, r) }
  | LE;       { fun l r -> Formula.Atom.Le  (l, r) }
  | GT;       { fun l r -> Formula.Atom.Gt  (l, r) }
  | GE;       { fun l r -> Formula.Atom.Ge  (l, r) }

let term :=
  | l = term; PLUS;  r = term;            <Formula.Term.Add>
  | l = term; MINUS; r = term;            <Formula.Term.Sub>
  | l = term; STAR;  r = term;            <Formula.Term.Mul>
  | MINUS; t = term;   %prec UMINUS       <Formula.Term.Neg>
  | n = INT;                              <Formula.Term.Const>
  | x = IDENT;                            <Formula.Term.Var>
  | t = parens(term);                     { t }
