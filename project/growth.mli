(* [company] represents the company in the Growth phase. *)
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

(** [f_event founded] gives the category and id of the current event *)
val f_event : company -> string * int

(** [set_f_event company category id] gives the [company] with the 
    event field set to [category] and [id]*)
val set_f_event : company -> string -> int -> company

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

(** [print_founded founded] displays the stats of [founded] to the player *)
val print_founded : company -> unit

(**[print_found_message msg] prints the instructions for starting the 2nd 
   phase of the game to the player. *)
val print_found_message : string -> unit


(** [print_updates prev_found new_founded] displays the stat changes in the
    change from [prev_found] to [new_founded] to the user. If no attributes
    are changed, nothing is printed. *) 
val print_updates : company -> company -> unit

(** [check_won_lost founded] checks the individual attributes of [founded] and
    returns a bool based on them. If any of {market_cap, morale, reputation, 
    management, marketing} are <= 0, then the game is ended and appropriate
    ending message is printed. *)
val check_won_lost : company -> bool


(**[check_won founded] checks the company's stats and sees if the winning 
   condition is met. If they are, prints the winning msg and returns true. 
   If not, prints nothing and returns false. *)
val check_won : company -> bool

(**[print_win_msg bool] prints the winning message if [bool] is true and
   returns it, otherwise prints nothing and returns [bool] *)
val print_win_msg : bool -> bool

(** [save] writes to or creates a JSON file with the save data of the 
    company. *)
val save : company -> unit

(** [load json] is the company represented in json. *)
val load : Yojson.Basic.t -> company