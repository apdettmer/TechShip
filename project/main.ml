open Yojson.Basic.Util

open Founding
open Growth
open Event

type load_or_delete = Load | Delete

(* indicates that a constructor event was selected and that event must be 
   handled differently to account for an employee or investor*)
exception Constructor 

type alternative = 
  | Response of response
  (* CResponse represents a response potentially containing an invesotr or
     an employee to be added to the company's stats*)
  | CResponse of response * (investor_or_employee option) 
  | FResponse of f_response
  | Status 
  | Save 
  | Menu 
  | Found

(** [alt_desc alt] gives a description of [alt] according to its type*)
let alt_desc alt =
  match alt with
  | Response res -> res_desc res
  | CResponse (res,_) -> res_desc res
  | FResponse f_res -> f_description f_res
  | Status -> "Display status."
  | Save -> "Save game."
  | Menu -> "Quit to main menu."
  | Found -> "Found your company for real: be done with such small affairs."

(** [pretty_print_alts altlst] displays each alternative in [altlst] 
    with a number in brackets and its description*)
let pretty_print_alts altlst = 
  List.iter (fun (i, a) -> 
      print_endline ("[" ^ (string_of_int i) ^ "] " ^ (alt_desc a))) altlst;
  print_string ">"

(** [present_alts company altlst event] displays alternatives in 
    [altlst] to the player and matches input against these alternatives *)
let rec present_alts company altlst event =
  pretty_print_alts altlst;
  try (
    match (List.assoc (read_int ()) altlst) with
    | Response res -> print_newline ();
      let updated_comp =  update_company res company event in 
      print_changes1 company updated_comp;
      play updated_comp
    | CResponse (res, inv_or_emp) -> print_newline ();
      let updated_comp = 
        update_company_constructor (res, inv_or_emp) company event in 
      print_changes1 company updated_comp;
      play updated_comp
    | Status -> print_newline (); 
      display_status company; 
      present_alts company altlst event
    | Save -> print_newline (); 
      let cat = category event in 
      let id = id event in 
      let new_comp = set_event company cat id in 
      save new_comp; 
      present_alts new_comp altlst event
    | Menu -> print_newline (); 
      main_menu ()
    | FResponse _ -> print_endline "Hey, that shouldn't have happened!";
      present_alts company altlst event
    | Found -> 
      print_found_message (company |> Founding.product |> string_of_product);
      Unix.sleepf 0.3 ;
      ANSITerminal.(print_string [green] ">herewegoround2.jpg

    ");
      try 
        company 
        |> found 
        |> (play_phase_2)
      with _ -> print_endline "here's your bug"
  )
  with _ -> print_string "Invalid entry.\n\n"; 
    present_alts company altlst event

and

  (** [default_alts len] gives recurring alternatives such as [Menu], [Save], 
      etc. used in the first phase, numbered according to [len]*)
  default_alts len = 
  (len, Found) :: 
  (len + 1, Status) ::
  (len + 2, Save) ::
  (len + 3, Menu) :: []

and
  (** [alts company reslst event] gives a list of alternatives based on 
      information from [reslst], [company], and [event]*)
  alts company reslst event =
  let l = List.length reslst in
  let altlst = 
    default_alts l @
    (List.combine (create_intlst [] l) (List.map (fun r -> Response r) reslst)) 
    |> List.sort (fun (i1, _) (i2, _) -> i1 - i2) 
  in present_alts company altlst event

and 

  (** [alts_constructor company res_cons_lst c_event] gives a list of 
      alternatives, specifically for responses handling employee or investor 
      addition*)
  alts_constructor company res_cons_lst c_event = 
  let len = List.length res_cons_lst in 
  let altlst = 
    default_alts len @
    (List.combine (create_intlst [] len) 
       (List.map (fun (r, emp_or_inv) -> 
            CResponse(r, emp_or_inv)) res_cons_lst))
    |> List.sort (fun (i1, _) (i2, _) -> i1 - i2) in
  let event = fst(c_event) in 
  print_endline (description event);
  present_alts company altlst event

and

  (**[display_event file] gives a random event generated from [file].
     Requires: [file] is a valid JSON representation of events for the game *)
  display_event file =
  let event_cat = random_category () in 
  if event_cat = "constructor" then raise Constructor
  else 
    let event = fill_event_description 
        ( Event.random_event file event_cat) 
        (select_some_word ()) 20 in
    print_endline (description event);
    event

and

  (**[play] is the repl loop that takes player input and determines actions
     in the game *)
  play company =
  if (check_lost_phase1 company) then exit 0
  else 
    try 
      let event = display_event "data/events.json" in
      let responses = responses event in
      alts company responses event
    with Constructor -> 
      let c_event = choose_constructor_event () in 
      let responses_and_addition = constructor_responses c_event in 
      alts_constructor company responses_and_addition c_event

and

  (**[play_from_save company] starts game session with the event being viewed 
     when [company] was last saved *)
  play_from_save company = 
  let event_info = event company in 
  if (fst event_info = "con_investor" || fst event_info = "con_employee")
  then let c_event = constructor_event_of (fst event_info) (snd event_info) in 
    let responses_and_addition = constructor_responses c_event in 
    alts_constructor company responses_and_addition c_event 
  else
    let event = event_of (fst(event_info)) (snd(event_info)) 
        "data/events.json" in
    print_endline (description event); 
    let responses = responses event in 
    alts company responses event

and

  (** [create_new_save ()] begins the game and prompts the player for input
      to create a new company and associated save file*)
  create_new_save () =
  ANSITerminal.(print_string [green] ">be you
>recent graduate of Cornell Engineering
>broke and jobless
>your parents lent you $5,000 and said \"get a life\"
>you have an idea for the Next Big Thing™:
>");
  let company = new_company (read_line ()) in
  ANSITerminal.(print_string [green] ">herewego.jpg

");
  save company;
  play company

and

  (** [handle_save_file load_or_delete file_name] either loads the game from 
      [file_name] if possible and if [load_or_delete] is value [Load], or 
      removes [file_name] from the current directory otherwise*)
  handle_save_file load_or_delete file_name =
  match load_or_delete with
  | Load -> print_newline (); 
    Yojson.Basic.from_file file_name 
    |> load 
    |> play_from_save
  | Delete -> print_newline (); 
    Sys.remove file_name; 
    main_menu ()

and

  (** [create_intlst acc i] appends to [acc] numbers from [0] to [i].
      Requuires: [i] is non-negative *)
  create_intlst acc i =
  match i - 1 with
  | 0 -> 0 :: acc
  | j -> create_intlst (j :: acc) (i - 1)

and

  (** [json_extension file] gives true if [file] is has the .json file extension
      and false otherwise*)
  json_extension file =
  let extension = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward extension file 0);
    true
  with _ -> false

and

  (** [list_files ()] gives a list of file names in the game directory with 
      the .json extension*)
  list_files () =
  Sys.readdir "." |> Array.to_list 
  |> List.filter (fun x -> json_extension x) 
  |> List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) 
  |> List.sort String.compare

and

  (** [display_save_file load_or_delete] lists the available save files, allows 
      the player to select one, and manipulates it depending on 
      [load_or_delete]. *)
  display_save_files load_or_delete =
  let save_files = list_files () in
  if save_files = [] then (
    print_string "No saves found.\n\n";
    main_menu ()
  )
  else print_endline "Select a save:";
  let nums = create_intlst [] (List.length save_files) in
  let opts = List.combine nums save_files in
  List.iter (fun (i, f) -> 
      print_endline ("[" ^ (string_of_int i) ^ "] " ^ f)) opts;
  print_string ">";
  try
    String.concat "" [(List.assoc (read_int ()) opts); ".json"] 
    |> handle_save_file load_or_delete
  with _ -> print_string "Invalid entry.\n\n";
    display_save_files load_or_delete

and

  (** [main_menu ()] prompts for the game to play, and then starts it and 
      displays the main menu. The main menu allows players to create a new save, 
      load a previous save, delete a previous save, or quit the game. *)
  main_menu () =
  print_string "

T E C H S H I P

[0] Create new save.
[1] Load save.
[2] Delete save.
[3] Quit.
>";
  match read_line () with
  | "0" -> print_newline (); 
    create_new_save ()
  | "1" -> print_newline (); 
    display_save_files Load
  | "2" -> print_newline (); 
    display_save_files Delete
  | "3" -> print_newline (); 
    exit 0
  | _ -> print_endline "Invalid entry."; 
    main_menu ()

and 

  (** [play_phase_2 founded]  is the repl for the second phase of the game *)
  play_phase_2 founded =
  Unix.sleep 1;
  if not (check_won_lost founded) then exit 0 
  else 
    let g_event = f_display_event "data/events_founded.json" in 
    let responses = responses g_event in 
    f_alts founded responses


and

  (** [f_alts founded res_lst] constructs a list of numbered choices for the
      the player in the second phase of the game*)
  f_alts founded res_lst = 
  let len = List.length res_lst in 
  (len, Status) ::
  (len + 1, Menu) ::
  (List.combine (create_intlst [] len) 
     (List.map (fun r -> FResponse r) res_lst))
  |> List.sort (fun (i1, a1) (i2, a2) -> i1 - i2) |> present_f_alts founded

and
  (** [present_f_alts founded altlst] displays options for the player and
      reads input specific to the second phase of the game *)
  present_f_alts founded altlst =
  pretty_print_alts altlst;
  try 
    match List.assoc (read_int ()) altlst with 
    | FResponse res -> print_newline ();
      let new_founded = update_founded founded res in 
      print_updates founded new_founded;
      play_phase_2 new_founded
    | Menu -> print_newline ();
      main_menu ()
    | Status -> print_founded founded; 
      present_f_alts founded altlst
    | _ -> print_endline "Unimplemented"
  with _ -> print_string "Invalid entry. \n\n";
    present_f_alts founded altlst

and

  (** [f_display_event file] selects an event from [file] at random, 
      prints its description and returns it*)
  f_display_event file =
  let event = fill_event_description
      ((f_random_category ()) 
       |> Event.random_event file)
      (select_some_word ()) 20 in
  print_endline (description event);
  event

(* Execute the game. *)
let () = main_menu ()