(* the type representing the founded company *)
type founded

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> founded

(** the type representing a response to an event from this phase *)
type f_response

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> founded

(**[product founded] is the product field of company [founded]*)
val product : founded -> Founding.product

(**[market_cap founded] is the market_cap field of company [founded]*)
val market_cap :  founded -> int 

(**[reputation founded] is the reputation field of company [founded]*)
val reputation : founded -> int

(**[morale founded] is the morale field of company [founded]*)
val morale: founded -> int

(**[teams founded] is the teams field of company [founded]*)
val teams : founded -> Founding.employee list list 

(**[investors founded] is the investors field of company [founded]*)
val investors : founded -> Founding.investor list

(**[date founded] is the date field of company [founded]*)
val date : founded -> Unix.tm

(**[marketing founded] is the marketing field of company [founded]*)
val marketing : founded -> int 

(**[management founded] is the management field of company [founded]*)
val management : founded -> int

(* (**[update_founded founded f_resp] takes a [founded] company and applies the 
   stat changes provided in [f_resp] and returns a new, updated company *)
   val update_founded : founded -> f_response -> founded *)
