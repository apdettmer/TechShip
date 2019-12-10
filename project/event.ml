open Printf
open Yojson.Basic.Util

open Founding

exception InvalidEventId of int

exception InvalidEventCategory of string


type response = {
  description : string; 
  effects : (string * int option) list;
}

(* type to handle constructor events *)
type investor_or_employee = 
    Employee of Founding.employee 
  | Investor of Founding.investor

(* the JSON file for events in the first phase*)
let event_file = Yojson.Basic.from_file "data/events.json"

let res_desc response = 
  response.description

let effects response = 
  response.effects

type event = {
  category : string;
  id : int;
  description : string; 
  stats : string list;
  responses : response list
} 

let category event = 
  event.category

let id event = 
  event.id

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
        id = id;
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

(** [make_event json category] gives an event directly from a JSON object
    Requires: [json] is a valid JSON represenation of an event*)
let make_event json category = {
  category = category;
  id = 
    json 
    |> member "id" 
    |> to_int;
  description = 
    json 
    |> member "description" 
    |> to_string;
  stats = 
    json
    |> member "stats"
    |> to_list
    |> get_str_lst [];
  responses = 
    json
    |> member "responses"
    |> to_list
    |> make_responses
}

let event_of category id file = 
  match member category (Yojson.Basic.from_file file) with 
  | `Null -> raise (InvalidEventCategory category)
  | c -> try let event = match_id category id (to_list c) in
      make_event event category
    with InvalidEventId i -> raise (InvalidEventId i)

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

(** [constructor_name json] gives the string associated with field name 
    if it is contained, or gives a random name otherwise*)
let constructor_name json = 
  match member "name" json with
  | `Null -> make_name () 
  | s -> to_string s

let make_employee_event () = 
  let emp_lst = 
    event_file
    |> member "constructor" 
    |> member "employee" 
    |> to_list in
  let ev = List.nth emp_lst (Random.int (List.length emp_lst)) in
  let e_name = constructor_name ev in
  let mor = member "morale" ev |> to_int in 
  let rep = member "reputatin" ev |> to_int in 
  let ev_ret =
    {
      category = "employee";
      id = ev |> member "id" |> to_int;
      description = 
        (Str.global_replace (Str.regexp "emp_name") e_name 
           (member "description" ev |> to_string));
      stats = [];
      responses = make_responses (member "responses" ev |> to_list)
    } 
  in (ev_ret, Employee (Founding.custom_employee e_name mor rep))

let make_investor_event () =  
  let inv_lst = 
    event_file 
    |> member "constructor" 
    |> member "investor" 
    |> to_list in
  let ev = List.nth inv_lst (Random.int (List.length inv_lst)) in 
  let i_name = constructor_name ev in 
  let invest = member "investment" ev |> to_int in 
  let ev_ret = 
    {
      category = "investor";
      id = member "id" ev |> to_int;
      description = 
        (Str.global_replace (Str.regexp "inv_name") i_name 
           (member "description" ev |> to_string));
      stats = [];
      responses = make_responses (member "responses" ev |> to_list)
    }
  in (ev_ret, Investor (Founding.custom_investor i_name invest))


let choose_constructor_event () = 
  Random.init (int_of_float (Unix.time ()));
  let choice = Random.int 2 in 
  if choice = 0 
  then make_investor_event ()
  else make_employee_event ()

let random_event file category = 
  Random.init (int_of_float (Unix.time ()));
  try 
    if file = "data/events.json" then
      (* if category = "constructor" then  *)
      let cat_actual = get_category category in
      let id = Random.int (List.length cat_actual) in 
      event_of category id file
    else if file = "data/events_founded.json" then
      let cat_actual = get_category_founded category in
      let id = Random.int (List.length cat_actual) in
      event_of_category_founded category id
    else 
      failwith "Entered an incorrect file name somewhere"
  with InvalidEventCategory _ ->  raise (InvalidEventCategory category)

let random_category () = 
  Random.init (int_of_float (Unix.time ()));
  match (Random.int 100) mod 5 with
  | 0 -> "constructor"
  | 1 -> "employee"
  | 2 -> "government"
  | 3 -> "other"
  | 4 -> "demo"
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

let print_category cat v = 
  match v with
  | a when a > 0 -> Stdlib.print_string ("(" ^ cat ^ "): "); 
    ANSITerminal.(print_string [green] ("+" ^ string_of_int v));
    Stdlib.print_endline ""
  | b when b < 0 -> Stdlib.print_string ("(" ^ cat ^ "): "); 
    ANSITerminal.(print_string [red] (string_of_int v)) ;
    Stdlib.print_endline ""
  | _ -> Stdlib.print_string ""

let print_changes1 old_comp new_comp = 
  print_category "funding" (funding new_comp - funding old_comp);
  print_category "reputation" (reputation new_comp - reputation old_comp);
  print_category "morale" (morale new_comp - morale old_comp);
  print_category "employees" 
    (List.length(employees new_comp) - List.length(employees old_comp));
  Stdlib.print_endline ""



let rec apply_effects company = function 
  | [] -> company
  | (category,opt)::t -> (
      match opt with 
      | None -> apply_effects company t
      | Some v -> apply_effects (update_category company category v) t
    )

let update_company (response : response) (company : Founding.company) event = 
  let updates = effects response in 
  (* print_changes updates; *)
  Stdlib.print_endline "";
  Unix.sleep 1;
  (* let new_comp = set_event company (category event) (id event) in  *)
  apply_effects company updates

(** [fill_description desc replace i] gives the [desc] with [replace] and
    [i] entered according to regular expression matching*)
let fill_description desc replace i = 
  (Str.global_replace (Str.regexp "string_val") replace desc)
  |> (Str.global_replace (Str.regexp "int_val") (string_of_int i))

let fill_event_description event replace i = 
  match event with | {category; id; description = d; stats; responses} ->
    {
      category;
      id;
      description = fill_description d replace i;
      stats;
      responses
    }

let select_some_word () =
  Random.init (int_of_float (Unix.time ()));
  let file = Yojson.Basic.from_file "data/wordbank.json" in
  let lst = member "words" file |> to_list in 
  List.nth lst (Random.int (List.length lst)) |> to_string

let rnd_employee () = 
  Founding.new_employee (make_name ())

(* (** [constructor_event id] gives an event from events.json from the
   constructor category under [subcategory] identifier [id]*)
   let constructor_event subcategory =
   let event_lst = 
   event_file 
   |> member "constructor"
   |> member subcategory
   |> to_list in 
   let lst_size = List.length event_lst in
   let event_num = Random.int lst_size in 
   let json_repn = List.nth event_lst event_num in 
   make_event json_repn subcategory  *)
