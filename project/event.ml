open Printf
open Yojson.Basic.Util

open Founding

exception InvalidEventId of int

exception InvalidEventCategory of string


type response = {
  description : string; 
  effects : (string * int option) list;
}

let res_desc response = response.description

let effects response = response.effects

type event = {
  category : string;
  description : string; 
  stats : string list;
  responses : response list
} 

let category event = 
  event.category

let description event = 
  event.description

let affected_stats event = 
  event.stats

let responses event = 
  event.responses

let new_response desc effects = {
  description = desc;
  effects = effects
}

(** [get_str_lst acc lst] converts [lst] of type Yojson.Basic.t list to
    a string list and appends to [acc].
    Requires: [lst] is a valid Yojson list of strings*)
let rec get_str_lst acc = function
  | [] -> acc
  | h::t -> let stat = h |> to_string in
    get_str_lst (stat::acc) t

(** [get_effect category response] gives [None] if there is no effect 
    associated with [category] under [response] or the value otherwise*)
let get_effect category response = 
  match member category response with
  | `Null -> None
  | v -> Some (to_int v)

(** [make_responses lst] gives a list of responses from [lst].
    Requires: [lst] is a valid list of responses as documented in events.json*)
let rec make_responses = function 
  | [] -> []
  | h::t -> 
    {
      description = h |> member "description" |> to_string;
      effects = ("funding", get_effect "funding" h) 
                :: ("reputation", get_effect "reputation" h) 
                :: ("morale", get_effect "morale" h) 
                :: ("employee", get_effect "employee" h) 
                :: []
    } :: make_responses t

let rec make_responses_founded = function
  | [] -> []
  | h :: t ->
    {
      description = h |> member "description" |> to_string;
      effects = ("market_cap", get_effect "market_cap" h) 
                :: ("reputation", get_effect "reputation" h) 
                :: ("morale", get_effect "morale" h) 
                :: ("investors", get_effect "investors" h) 
                :: ("teams", get_effect "teams" h )
                :: ("marketing", get_effect "marketing" h)
                :: ("management", get_effect "management" h)
                :: []
    } :: make_responses_founded t

(** [match_id category id lst] gives the json event that matches
    [id].
    Raises: [InvalidEvent] if [id] is unmatched
    Requires: [lst] is valid list of events, category is a
    category in events.json*)
let rec match_id category id = function 
  | [] -> 
    raise (InvalidEventId id)
  | h::t -> if h |> member "id" |> to_int = id then h 
    else match_id category id t

let get_category_founded category = 
  let file = Yojson.Basic.from_file "data/events_founded.json" in
  match member category file with
  | `Null -> raise (InvalidEventCategory category)
  | c -> to_list c

let get_category category =
  match member category (Yojson.Basic.from_file "data/events.json") with
  | `Null -> raise (InvalidEventCategory category)
  | c -> to_list c

let event_of_category_founded category id = 
  let file = Yojson.Basic.from_file "data/events_founded.json" in
  match member category file with 
  | `Null -> raise (InvalidEventCategory category)
  | c -> try let event = match_id category id (to_list c) in
      {
        category = category;
        description = 
          event 
          |> member "description" 
          |> to_string;
        stats = 
          event 
          |> member "stats" 
          |> to_list 
          |> get_str_lst [];
        responses = 
          event 
          |> member "responses" 
          |> to_list 
          |> make_responses_founded
      }
    with InvalidEventId i -> raise (InvalidEventId i)

let event_of category id file = 
  match member category (Yojson.Basic.from_file file) with 
  | `Null -> raise (InvalidEventCategory category)
  | c -> try let event = match_id category id (c |> to_list) in
      {
        category = category;
        description = 
          event 
          |> member "description" 
          |> to_string;
        stats = 
          event 
          |> member "stats" 
          |> to_list 
          |> get_str_lst [];
        responses = 
          event 
          |> member "responses" 
          |> to_list 
          |> make_responses
      }
    with InvalidEventId i -> raise (InvalidEventId i)

let random_event file category = 
  Random.init (int_of_float (Unix.time ()));
  try let cat_actual = get_category category in
    let id = Random.int (List.length cat_actual) in 
    event_of category id file
  with InvalidEventCategory _ ->  raise (InvalidEventCategory category)

let random_category () = 
  Random.init (int_of_float (Unix.time ()));
  match (Random.int 100) mod 4 with
  | 0 -> "investor"
  | 1 -> "employee"
  | 2 -> "government"
  | 3 -> "other"
  | _ -> "other"

(* TODO as events added*)
let f_random_category () = 
  "demo" 

let rec print_changes effects = 
  match effects with 
  | [] -> ()
  | (name, Some (v)) :: t -> 
    Stdlib.print_string ("(" ^ name ^ "): ");
    if v >= 0 then ANSITerminal.(print_string [green] ("+" ^ string_of_int v))
    else ANSITerminal.(print_string [red] (string_of_int v));
    Stdlib.print_endline "";
    print_changes t
  | _ -> ()


let rec apply_effects company = function 
  | [] -> company
  | (category,opt)::t -> (
      match opt with 
      | None -> apply_effects company t
      | Some v -> apply_effects (update_category company category v) t
    )

let update_company (response : response) (company : Founding.company) = 
  let updates = effects response in 
  print_changes updates;
  Stdlib.print_endline "";
  Unix.sleep 1;
  apply_effects company updates

(** [fill_description desc replace i] gives the [desc] with [replace] and
    [i] entered according to regular expression matching*)
let fill_description desc replace i = 
  (Str.global_replace (Str.regexp "string_val") replace desc)
  |> (Str.global_replace (Str.regexp "int_val") (string_of_int i))

let fill_event_description event replace i = 
  match event with | {category; description = d; stats; responses} ->
    {
      category;
      description = fill_description d replace i;
      stats;
      responses
    }

let select_some_word () =
  Random.init (int_of_float (Unix.time ()));
  let file = Yojson.Basic.from_file "data/wordbank.json" in
  let lst = member "words" file |> to_list in 
  List.nth lst (Random.int (List.length lst)) |> to_string


let make_name () =
  Random.init (int_of_float (Unix.time ()));
  let file = Yojson.Basic.from_file "data/wordbank.json" in
  let fst_name_lst = member "first names" file |> to_list in
  let fst_name = 
    List.nth fst_name_lst (Random.int (List.length fst_name_lst))
    |> to_string in 
  let last_name_lst = member "last names" file |> to_list in
  let last_name =
    List.nth last_name_lst (Random.int (List.length last_name_lst))
    |> to_string in fst_name ^ " " ^ last_name

let rnd_employee () = 
  Founding.new_employee (make_name ()) 

let make_employee_event () =
  let e_name = make_name () in 
  let file = Yojson.Basic.from_file "data/events.json" in 
  let emp_lst = file |> member "constructor" |> member "employee" |> to_list in
  let ev = List.nth emp_lst (Random.int (List.length emp_lst)) in 
  {
    category = "employee";
    description = 
      (Str.global_replace (Str.regexp "emp_name") e_name 
         (member "description" ev |> to_string));
    stats = [];
    responses = make_responses (member "responses" ev |> to_list)
  }, Founding.new_employee e_name

(* let make_investor_event () = 
   let i_name = make_name () in 
   let file = Yojson.Basic.from_file "data/events.json" in 
   let inv_lst = file |> member "constructor" |> member "investor" |> to_list in
   let ev = List.nth inv_lst (Random.int (List.length inv_lst)) in 
   {
    category = "investor";
    description = 
      (Str.global_replace (Str.regexp "emp_name") i_name 
         (member "description" ev |> to_string));
    stats = [];
    responses = make_responses (member "responses" ev |> to_list)
   }, Founding.new_employee i_name *)