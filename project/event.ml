open Yojson.Basic.Util

exception InvalidEvent of (string * int) * string
(* the type representing responses*)
type response = {description : string; effects : (string * int) list}

type e = {category : string; description : string; stats : string list;
          responses : response list } 

let category event = event.category

let description event = event.description

let affected_stats event = event.stats

let responses event = event.responses

(** [get_str_lst acc lst] converts [lst] of type Yojson.Basic.t list to
    a string list and appends to [acc].
    Requires: [lst] is a valid Yojson list of strings*)
let rec get_str_lst acc = function
  | [] -> acc
  | h::t -> let stat = h |> to_string in
    get_str_lst (stat::acc) t

(** [make_responses lst] gives a list of responses from [lst].
    Requires: [lst] is a valid list of responses as documented in events.json*)
let rec make_responses = function 
  | [] -> []
  | h::t -> 
    {description = h |> member "description" |> to_string;
     effects = []} :: make_responses t

(** [match_id category id lst] gives the json event that matches
    [id].
    Raises: [InvalidEvent] if [id] is unmatched
    Requires: [lst] is valid list of events, category is a
    category in events.json*)
let rec match_id category id = function 
  | [] -> raise (InvalidEvent((category, id), "Invalid id"))
  | h::t -> if h |> member "id" |> to_int = id then h 
    else match_id category id t

let event_of category id = 
  match member category (Yojson.Basic.from_file "events.json") with 
  | `Null -> raise (InvalidEvent ((category, id), "Invalid category"))
  | c -> try let event = match_id category id (c |> to_list) in
      {
        category = category;
        description = event |> member "description" |> to_string;
        stats = event |> member "stats" 
                |> to_list |> get_str_lst [];
        responses = event |> member "responses" |> to_list |> make_responses
      }
    with InvalidEvent _ -> raise (InvalidEvent ((category, id), "Invalid id"))



