open Yojson.Basic.Util

open Founding
open Growth
open Event

type load_or_delete = Load | Delete

type alternative = 
  | Response of response 
  | FResponse of f_response
  | Status 
  | Save 
  | Menu 
  | Found

let alt_desc alt =
  match alt with
  | Response res -> res_desc res
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

let rec present_alts company altlst event =
  pretty_print_alts altlst;
  try (
    match (List.assoc (read_int ()) altlst) with
    | Response res -> print_newline (); 
      update_company res company event |> play
    | Status -> print_newline (); 
      display_status company; 
      present_alts company altlst event
    | Save -> print_newline (); 
      save company; 
      present_alts company altlst event
    | Menu -> print_newline (); 
      main_menu ()
    | FResponse _ -> print_endline "Hey, that shouldn't have happened!";
      present_alts company altlst event
    | Found -> 
      print_found_message (company |> Founding.product |> string_of_product);
      Unix.sleep 1;
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

  alts company reslst event =
  let l = List.length reslst in
  let altlst = 
    (l, Found) ::
    (l + 1, Status) :: 
    (l + 2, Save) :: 
    (l + 3, Menu) :: 
    (List.combine (create_intlst [] l) (List.map (fun r -> Response r) reslst)) 
    |> List.sort (fun (i1, a1) (i2, a2) -> i1 - i2) 
  in present_alts company  altlst event

and

  (**[display_event file] generates a random event of type [e] from 
     JSON file [file], prints out its description and returns it. *)
  display_event file =
  let event = fill_event_description 
      ((random_category ()) 
       |> Event.random_event file) 
      (select_some_word ()) 20 in
  print_endline (description event);
  event

and

  (**[play] is the repl loop that takes player input and determines actions
     in the game. [player_file] is a JSON file that is a save file. *)
  play company =
  let event = display_event "data/events.json" in
  let responses = responses event in
  alts company responses event
and
  (**[play_from_save company] starts game session with the event being viewed 
     when [company] was last saved *)

  play_from_save company = 
  let event_info = event company in 
  let event = event_of (fst(event_info)) (snd(event_info)) "data/events.json" in
  print_endline (description event); 
  let responses = responses event in 
  alts company responses event

and

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

  create_intlst acc i =
  match i - 1 with
  | 0 -> 0 :: acc
  | j -> create_intlst (j :: acc) (i - 1)

and

  json_extension file =
  let extension = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward extension file 0);
    true
  with _ -> false

and

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