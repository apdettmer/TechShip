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


