open Yojson.Basic.Util
open Printf

exception InvalidEventId of int

exception InvalidEventCategory of string

(* the type representing responses*)
type response = {
  description : string; 
  effects : (string * int option) list;
}

let res_desc response = response.description

let effects response = response.effects

type event = {category : string; description : string; stats : string list;
              responses : response list } 

let category event = 
  event.category

let description event = 
  event.description

let affected_stats event = 
  event.stats

let responses event = 
  event.responses

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
    {description = h |> member "description" |> to_string;
     effects = ("funding", get_effect "funding" h) :: 
               ("reputation", get_effect "reputation" h) ::
               ("morale", get_effect "morale" h) :: []} :: make_responses t

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

let get_category category =
  match member category (Yojson.Basic.from_file "data/events.json") with
  | `Null -> raise (InvalidEventCategory category)
  | c -> to_list c

let event_of category id = 
  match member category (Yojson.Basic.from_file "data/events.json") with 
  | `Null -> raise (InvalidEventCategory category)
  | c -> try let event = match_id category id (c |> to_list) in
      {
        category = category;
        description = event |> member "description" |> to_string;
        stats = event |> member "stats" 
                |> to_list |> get_str_lst [];
        responses = event |> member "responses" |> to_list |> make_responses
      }
    with InvalidEventId i -> raise (InvalidEventId i)

(* let rec add_effects company = function
   | [] -> company
   | (c,v) :: t -> add_effects  *)


let random_event category = 
  Random.init (int_of_float (Unix.time ()));
  try let cat_actual = get_category category in
    let id = Random.int (List.length cat_actual) in 
    event_of category id 
  with InvalidEventCategory _ ->  raise (InvalidEventCategory category)

let rec sum_func lst company =
  match lst with
  | [] -> company
  | (cat, amount)::t -> failwith "Unimplemented"

(**[]  *)
let random_category (company : Founding.company) = 
  Random.init (int_of_float (Unix.time ()));
  match (Random.int 100) mod 4 with
  | 0 -> "investor"
  | 1 -> "employee"
  | 2 -> "government"
  | 3 -> "other"
  | _ -> "other"

(* let rec apply_effects company = function
   | ("funding", i) :: t -> { product = product (company); 
                             funding = funding (company) + i; reputation = reputation(company); 
                             morale = morale (company); employees = employees (company); 
                             investors = investors (company); date = date (company)
                           }
   | ("morale", i) :: t-> company
   | ("reputation", i) :: t -> company 
   | ("employees", i) :: t-> company
   | (_ , i)  :: t -> apply_effects company t
   | [] -> company *)

let rec apply_effects company = function 
  | [] -> company
  | (category,opt)::t -> begin
      match opt with 
      | None -> apply_effects company t
      | Some v -> apply_effects (Founding.update_category company category v) t
    end


let update_company (response : response) (company : Founding.company) = 
  let updates = effects response in 
  apply_effects company updates

(* apply_effects company (effects response) *)

let fill_event_description event replace i = 
  match event with | {category; description = d; stats; responses} ->
    {
      category;
      description =

        (Str.global_replace (Str.regexp "string_val") replace d)
        |> (Str.global_replace (Str.regexp "int_val") (string_of_int i));
      stats;
      responses
    }

let select_some_word () =
  Random.init (int_of_float (Unix.time ()));
  let file = Yojson.Basic.from_file "data/samplewords.json" in
  let lst = member "words" file |> to_list in 
  List.nth lst (Random.int (List.length lst)) |> to_string