(* mli for an event *)


type event

(* the type representing responses to events*)
type response 

(* type to used handle events that also construct investors or employees,
   as defined in founding.ml *)
type investor_or_employee 

exception InvalidEventId of int
exception InvalidEventCategory of string
exception WrongPersonType of string

(** [inv_from_var inv_wrapped] gives the investor contained in [inv_wrapped].
    Raises: [WrongPersonType("employee")] if [inv_wrapped] represents an 
    employee*)
val inv_from_var : investor_or_employee -> Founding.investor

(** [emp_from_var emp_wrapped] gives the investor contained in [inv_wrapped].
    Raises: [WrongPersonType("investor")] if [emp_wrapped] represents an 
    investor*)
val emp_from_var : investor_or_employee -> Founding.employee

(** [category event] gives the category of [event]*)
val category : event -> string

(** [description event] gives the description of [event]*)
val description : event -> string

(** [id event] gives the identifier of [event] within its category*)
val id : event -> int

(** [affected_stats event] gives all the company stats potentially affected
    by [event]*)
val affected_stats : event -> string list

(**[responses event] is the list of possible [responses] to that event *)
val responses : event -> response list

(**[new_response desc effects add] is a new response with the 
   description [desc], effects field [effects], and add field [add] *)
val new_response : string -> (string * int option) list -> bool -> response

(**[response_description response] is the description of [response]*)
val res_desc : response -> string

(** [effects event] is the list of integer effects on the company from 
    [event]*)
val effects : response -> (string * int option) list

(** [add response] indicates whether [response] is used to add an 
    employee or investor to the company*)
val add : response -> bool

(** [print_changes1 old_comp new_comp] prints to the user the stat changes
    between the [old_comp] and the [new_comp]. *)
val print_changes1 : Founding.company -> Founding.company -> unit

(**  [update_company response company event] gives the company with the effects
     of [response] applied to [company]*)
val update_company : response -> Founding.company -> event -> Founding.company

(** [event_of category id file] gives the event with [id] in [category] from
    JSON [file]
    Raises: [InvalidEvent] if [category] is not a category of event in
    events.json or if [id] is not the id of an event in [category]*)
val event_of : string -> int -> string -> event

(** [random_event file category] selects a random event in [category] under
    JSON [file].
    Raises: [InvalidEventCategory] if [category] is not in [file]*)
val random_event : string -> string -> event

(** [random_category ()] selects at random a category associated with the
    first phase of the game*)
val random_category : unit -> string

(** [f_random_category ()] selects at random a category associated with the
    second phase of the game *)
val f_random_category : unit -> string

(** [fill_event_description event replace i] gives the description of
    [event] with all instances of 'string_val' and 'int_val' replaced with 
    [replace] and [i] respectively*)
val fill_event_description : event -> string -> int -> event

(** [select_some_word ()] gives a word from the 'words' category in 
    wordbank.json*)
val select_some_word : unit -> string

(** [make_name ()] constructs a two-part name from first and last names in
    wordbank.json*)
val make_name : unit -> string

(** [rnd_employee ()] gives an employee with a randomly generated name using 
    names from wordbank.json*)
val rnd_employee : unit -> Founding.employee

(** [make_employee_event ()] gives a randomly generated employee based on info
    from events.json*)
val make_employee_event : unit -> event * investor_or_employee

(** [make_employee_event ()] gives a randomly generated employee based on info
    from events.json*)
val make_investor_event : unit -> event * investor_or_employee

(** [choose_constructor_event ()] gives a randomly selected event from the 
    constructor category, along with the investor or employee created from the
    event *)
val choose_constructor_event : unit -> event * investor_or_employee

(** [constructor_responses c_event] gives the responses to the event
    associated with [c_event] coupled with [Some v], where [v] is an 
    investor or employee, if the response adds an investor or employee, 
    or [None] otherwise*)
val constructor_responses : 
  event * investor_or_employee -> (response * investor_or_employee option) list

(** [update_company_constructor c_response company event] gives a company
    with the stats of [company] altered according to [c_response], potentially 
    adding an investor or employee to [company]*)
val update_company_constructor : 
  (response * investor_or_employee option) -> Founding.company -> event 
  -> Founding.company

(** [constructor_event_of category id] gives the event and investor or employee
    associated with [category] and [id] in events.json
    Raises: [InvalidEventCategory category] if [category] is not a subcategory 
    of constructor,
    [InvalidEventId id] if [id] is not the identifier of an event in 
    [category]*)
val constructor_event_of : string -> int -> 
  (event * investor_or_employee)


