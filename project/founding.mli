(* design the company in the growth phase *)
type company

type employee

type product

type investor

(**  *)
val product : company -> product

(**  *)
val funding : company -> int

(**  *)
val employees : company -> employee list

(**  *)
val reputation : company -> int

(**  *)
val morale : company -> int

(**  *)
val investors : company -> investor list

val print_stats: company -> unit


