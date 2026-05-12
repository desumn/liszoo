let paren_if cond ppf do_ =
  if cond then (Fmt.pf ppf "("; do_ (); Fmt.pf ppf ")")
  else do_ ()
