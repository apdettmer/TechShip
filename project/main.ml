open Yojson.Basic.Util

open Founding
open Growth
open Event

type load_or_delete = Load | Delete

type alternative = Response of response | Status | Save | Menu 

let alt_desc alt =
  match alt with
  | Response res -> res_desc res
  | Status -> "Display status."
  | Save -> "Save game."
  | Menu -> "Quit to main menu."

let rec present_alts company altlst =
  List.iter (fun (i, a) -> print_endline ("[" ^ (string_of_int i) ^ "] " ^ (alt_desc a))) altlst;
  print_string ">";
  try (
    match (List.assoc (read_int ()) altlst) with
    | Response res -> print_newline (); update_company res company |> play
    | Status -> print_newline (); display_status company; present_alts company altlst
    | Save -> print_newline (); save company; present_alts company altlst
    | Menu -> print_newline (); main_menu ()
  )
  with _ -> print_string "Invalid entry.\n\n"; present_alts company altlst

and

  alts company reslst =
  let l = List.length reslst in
  (l, Status) :: (l + 1, Save) :: (l + 2, Menu) :: (List.combine (create_intlst [] l) (List.map (fun r -> Response r) reslst)) |> List.sort (fun (i1, a1) (i2, a2) -> i1 - i2) |> present_alts company

and

  (**[display_event company] generates a random event of type [e], prints out
     its description and returns it. *)
  display_event company =
  let event = fill_event_description ((*company |> random_category*) "demo" |> Event.random_event) (select_some_word ()) 20 in
  print_endline (description event);
  event

and

  (**[play] is the repl loop that takes player input and determines actions
     in the game. [player_file] is a JSON file that is a save file. *)
  play company =
  let event = display_event company in
  let responses = responses event in
  alts company responses

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
  | Load -> print_newline (); Yojson.Basic.from_file file_name |> load |> play
  | Delete -> print_newline (); Sys.remove file_name; main_menu ()

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
  Sys.readdir "." |> Array.to_list |> List.filter (fun x -> json_extension x) |> List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) |> List.sort String.compare

and

  (** [display_save_file load_or_delete] lists the available save files, allows the player to select one, and manipulates it depending on [load_or_delete]. *)
  display_save_files load_or_delete =
  let save_files = list_files () in
  if save_files = [] then (
    print_string "No saves found.\n\n";
    main_menu ()
  )
  else print_endline "Select a save:";
  let nums = create_intlst [] (List.length save_files) in
  let opts = List.combine nums save_files in
  List.iter (fun (i, f) -> print_endline ("[" ^ (string_of_int i) ^ "] " ^ f)) opts;
  print_string ">";
  try
    String.concat "" [(List.assoc (read_int ()) opts); ".json"] |> handle_save_file load_or_delete
  with _ -> print_string "Invalid entry.\n\n";
    display_save_files load_or_delete

and

  (** [main_menu ()] prompts for the game to play, and then starts it and displays the main menu. The main menu allows players to create a new save, load a previous save, delete a previous save, or quit the game. *)
  main_menu () =
  print_string "

T E C H S H I P

[0] Create new save.
[1] Load save.
[2] Delete save.
[3] Quit.
>";
  match read_line () with
  | "0" -> print_newline (); create_new_save ()
  | "1" -> print_newline (); display_save_files Load
  | "2" -> print_newline (); display_save_files Delete
  | "3" -> print_newline (); exit 0
  | _ -> print_endline "Invalid entry."; main_menu ()

(* Execute the game. *)
let () = main_menu ()