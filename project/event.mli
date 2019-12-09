(* mli for an event *)


type event

type response


(* following types unneccesary? Can match categories just by string, and this
   is only useful for getting an event -- not sure what other information we'd
   need to carry other than potentially the name of an investor*)
(* type investor

   type government

   type employee

   type event_type = Investor of investor 
                | Government of government 
                | Employee of employee *)

exception InvalidEventId of int
exception InvalidEventCategory of string

(** [category event] gives the category of [event]*)
val category : event -> string

(** [description event] gives the description of [event]*)
val description : event -> string

(** [affected_stats event] gives all the company stats potentially affected
    by [event]*)
val affected_stats : event -> string list

(**[responses event] is the list of possible [responses] to that event *)
val responses : event -> response list

(**[response_description response] is the description of [response]*)
val res_desc : response -> string

(** [effects event] is the list of integer effects on the company from 
    [event]*)
val effects : response -> (string * int option) list



(**  [update_company response company] gives the company with the effects
     of [response] applied to [company]*)
val update_company : response -> Founding.company -> Founding.company

(** [event_of category id] gives the event with [id] in [category] from
    events.json
    Raises: [InvalidEvent] if [category] is not a category of event in
    events.json or if [id] is not the id of an event in [category]*)
val event_of : string -> int -> event

(** [random_event category] selects a random event from [category].
    Raises: [InvalidEventCategory] if [category] is not in events.json*)
val random_event : string -> event

(** [random_category company] selects at random a category in {"investor", "other", 
    "employee", "government"}. As of 11/19, the implementation does not factor in 
    the company's stats for the randomness.  *)
val random_category : Founding.company -> string

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
val make_employee_event : unit -> event * Founding.employee
