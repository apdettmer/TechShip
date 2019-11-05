(* mli for an event *)


type e

type investor

type government

type employee

type event_type = Investor of investor 
                | Government of government 
                | Employee of employee

type response


(**  *)
val event_type : e -> event_type

(**  *)
val update_company : e -> int -> Founding.company

(** [event_of category id] gives the event with [id] in [category] from
    events.json
    Raises: [InvalidEvent] if [category] is not a category of event in
    events.json or if [id] is not the id of an event in [category]*)
val event_of : string -> int -> e


