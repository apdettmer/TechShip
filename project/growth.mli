(* the type representing the founded company *)
type founded

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> founded
