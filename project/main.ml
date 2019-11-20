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
    | Response res -> print_endline ""; update_company res company |> play
    | Status -> print_endline ""; display_status company; present_alts company altlst
    | Save -> print_endline ""; save company; present_alts company altlst
    | Menu -> print_endline ""; main_menu ()
  )
  with _ -> print_string "Invalid entry.\n\n"; present_alts company altlst

and

  create_intlst acc l =
  match l - 1 with
  | 0 -> 0 :: acc
  | n -> create_intlst (n :: acc) (l - 1)

and

  alternatives company reslst =
  let l = List.length reslst in
  (l, Status) :: (l + 1, Save) :: (l + 2, Menu) :: (List.combine (create_intlst [] l) (List.map (fun r -> Response r) reslst)) |> List.sort (fun (i1, a1) (i2, a2) -> i1 - i2) |> present_alts company

and

  (**[display_event company] generates a random event of type [e], prints out
     its description and returns it. *)
  display_event company =
  let event = fill_event_description ("demo" |> Event.random_event) (select_some_word ()) 20 in
  print_endline (description event);
  event

and

  (**[play] is the repl loop that takes player input and determines actions
     in the game. [player_file] is a JSON file that is a save file. *)
  play company =
  let event = display_event company in
  let responses = responses event in 
  alternatives company responses

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

  json_extension file =
  let extension = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward extension file 0);
    true
  with _ -> false

and

  handle_save_file_helper load_or_delete file_name =
  match load_or_delete with
  | Load -> print_endline ""; Yojson.Basic.from_file file_name |> load |> play
  | Delete -> print_endline ""; Sys.remove file_name; main_menu ()

and

  list_files () =
  Sys.readdir "." |> Array.to_list |> List.filter (fun x -> json_extension x) |> List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) |> List.sort String.compare

and

  handle_save_file load_or_delete =
  print_endline "Select a save:";
  let save_files = list_files () in
  let nums = create_intlst [] (List.length save_files) in 
  let vals = List.combine nums save_files in 
  List.iter (fun (i,f) -> 
      print_endline ("[" ^ (string_of_int i) ^ "] " ^ f)) vals;
  print_string ">";
  try (String.concat "" [(List.assoc (read_int ()) vals); ".json"] |> handle_save_file_helper load_or_delete)
  with _ -> print_string "Invalid entry.\n\n";
    handle_save_file load_or_delete

and

  main_menu_helper () =
  print_string "Invalid entry.

T E C H S H I P

[0] Create new save.
[1] Load save.
[2] Delete save.
[3] Quit.
>";
  match read_line () with
  | "0" -> print_endline ""; create_new_save ()
  | "1" -> print_endline ""; handle_save_file Load
  | "2" -> print_endline ""; handle_save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

and

  (** [main ()] prompts for the game to play, then starts it. *)
  main_menu () =
  print_string "

T E C H S H I P

[0] Create new save.
[1] Load save.
[2] Delete save.
[3] Quit.
>";
  match read_line () with
  | "0" -> print_endline ""; create_new_save ()
  | "1" -> print_endline ""; handle_save_file Load
  | "2" -> print_endline ""; handle_save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

(* Execute the game. *)
let () = main_menu ()