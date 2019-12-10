(* the type representing the founded company *)
type founded

(** the type representing a response to an event from this phase *)
type f_response = Event.response

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> founded

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

(** [event founded] gives the category and id of the current event *)
(* val event : founded -> string * int *)

(**[f_effects f_resp] is the effects field of given f_response [f_resp] *)
val f_effects : f_response -> (string * int option) list

(**[f_description f_resp] is the description of given f_response [f_resp]  *)
val f_description : f_response -> string

(**[new_f_response desc effects] is a new response with the description [desc]
   and effects field [effects] *)
val new_f_response : string -> (string * int option) list -> f_response 

(**[update_founded founded f_resp] takes a [founded] company and applies the 
   stat changes provided in [f_resp] and returns a new, updated company *)
val update_founded : founded -> f_response -> founded 

val print_founded : founded -> unit

val print_found_message : string -> unit

val print_updates : founded -> founded -> unit

val check_won_lost : founded -> bool