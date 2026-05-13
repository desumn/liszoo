
type smt_result = Valid | Invalid | Unknown of [`Unknown | `No_result]

val check_alt_ergo : Formula.t -> (smt_result, [`Msg of string]) result 
