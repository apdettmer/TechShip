(* mli for an event *)


type e

type response

type subresponse
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
val category : e -> string

(** [description event] gives the description of [event]*)
val description : e -> string

(** [affected_stats event] gives all the company stats potentially affected
    by [event]*)
val affected_stats : e -> string list


val responses : e -> response list

(* * 
   val event_type : e -> event_type *)

(**  *)
val update_company : e -> int -> Founding.company -> Founding.company

(** [event_of category id] gives the event with [id] in [category] from
    events.json
    Raises: [InvalidEvent] if [category] is not a category of event in
    events.json or if [id] is not the id of an event in [category]*)
val event_of : string -> int -> e

(** [random_event category] selects a random event from [category].
    Raises: [InvalidEventCategory] if [category] is not in events.json*)
val random_event : string -> e