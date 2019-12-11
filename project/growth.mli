(* the type representing the founded company *)
type company

(** the type representing a response to an event from this phase *)
type f_response = Event.response

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> company

(** [found company] gives a new founded company using stats from
    [company] *)
val found : Founding.company -> company

(**[product founded] is the product field of company [founded]*)
val product : company -> string

(**[market_cap founded] is the market_cap field of company [founded]*)
val market_cap :  company -> int 

(**[reputation founded] is the reputation field of company [founded]*)
val reputation : company -> int

(**[morale founded] is the morale field of company [founded]*)
val morale: company -> int

(**[teams founded] is the teams field of company [founded]*)
val teams : company -> Founding.employee list list 

(**[investors founded] is the investors field of company [founded]*)
val investors : company -> Founding.investor list

(**[date founded] is the date field of company [founded]*)
val date : company -> Unix.tm

(**[marketing founded] is the marketing field of company [founded]*)
val marketing : company -> int 

(**[management founded] is the management field of company [founded]*)
val management : company -> int

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
val update_founded : company -> f_response -> company 

val print_founded : company -> unit

val print_found_message : string -> unit

val print_updates : company -> company -> unit

val check_won_lost : company -> bool

(** [save] writes to or creates a JSON file with the save data of the 
    company. *)
val save : company -> unit

(** [load json] is the company represented in json. *)
val load : Yojson.Basic.t -> company