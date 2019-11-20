open Yojson.Basic.Util

open Founding
open Growth
open Event

type load_or_delete = Load | Delete

type alternative = Response of response | Status | Save | Menu

let alternative_description alt =
  match alt with
  | Response res -> response_description res
  | Status -> "Display status."
  | Save -> "Save game."
  | Menu -> "Quit to main menu."

let rec present_alternatives company altlst =
  List.iter (fun (i, a) -> print_endline ("[" ^ (string_of_int i) ^ "] " ^ (alternative_description a))) altlst;
  print_string ">";
  match (List.assoc (read_int ()) altlst) with
  | Response res -> update_company res company |> play
  | Status -> display_status company; present_alternatives company altlst
  | Save -> save company; present_alternatives company altlst
  | Menu -> (* main_menu () *) present_alternatives company altlst

and

  create_ilst acc l =
  match l - 1 with
  | 0 -> 0 :: acc
  | n -> create_ilst (n :: acc) (l - 1)

and

  alternatives company reslst =
  let l = List.length reslst in
  (l, Status) :: (l + 1, Save) :: (l + 2, Menu) :: (List.combine (create_ilst [] l) (List.map (fun r -> Response r) reslst)) |> List.sort (fun (i1, a1) (i2, a2) -> i1 - i2) |> present_alternatives company

and

  (**[display_event company] generates a random event of type [e], prints out
     its description and returns it. *)
  display_event company = 
  let e = company |> Event.random_category |> Event.random_event in
  Stdlib.print_endline (description e); Stdlib.print_endline "";
  e

and

  (**[play] is the repl loop that takes player input and determines actions
     in the game. [player_file] is a JSON file that is a save file. *)
  play company =
  let event = display_event company in
  let responses = responses event in 
  alternatives company responses

and

  create_new_save () =
  ANSITerminal.(print_string [green] ">be you\n");
  ANSITerminal.(print_string [green] ">recent graduate of Cornell Engineering\n");
  ANSITerminal.(print_string [green] ">broke and jobless\n");
  ANSITerminal.(print_string [green] ">your parents lent you $5,000 and said \"get a life\"\n");
  ANSITerminal.(print_string [green] ">you have an idea for the Next Big Thing™:\n");
  ANSITerminal.(print_string [green] ">");
  let company = new_company (read_line ()) in
  ANSITerminal.(print_string [green] ">herewego.jpg\n");
  save company;
  play company
(* print_endline "mark" *)

and

  json_extension file =
  let extension = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward extension file 0);
    true
  with Not_found -> false

and

  handle_save_file_helper load_or_delete file_name =
  match load_or_delete with
  | Load -> Yojson.Basic.from_file file_name |> load |> play
  | Delete -> Sys.remove file_name

and

  list_files () =
  Sys.readdir "." |> Array.to_list |> List.filter (fun x -> json_extension x) |> List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) |> List.sort String.compare

and

  find_file load_or_delete save_files input=
  if List.mem input save_files
  then String.concat "" [input; ".json"] |> handle_save_file_helper load_or_delete
  else (print_endline "Invalid entry.";
        print_endline "";
        handle_save_file load_or_delete)

and

  handle_save_file load_or_delete =
  print_endline "Select a save:";
  let save_files = list_files () in
  List.iter print_endline save_files;
  print_string ">";
  read_line () |> find_file load_or_delete save_files

and

  main_menu_helper () =
  print_endline "Invalid entry.";
  print_endline "";
  print_endline "[0] Create new save.";
  print_endline "[1] Load save.";
  print_endline "[2] Delete save.";
  print_endline "[3] Quit.";
  print_string ">";
  match read_line () with
  | "0" -> print_endline ""; create_new_save ()
  | "1" -> print_endline ""; handle_save_file Load
  | "2" -> print_endline ""; handle_save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

and

  (** [main ()] prompts for the game to play, then starts it. *)
  main_menu () =
  print_endline "";
  print_endline "";
  print_endline "T E C H S H I P";
  print_endline "";
  print_endline "[0] Create new save.";
  print_endline "[1] Load save.";
  print_endline "[2] Delete save.";
  print_endline "[3] Quit.";
  print_string ">";
  match read_line () with
  | "0" -> print_endline ""; create_new_save ()
  | "1" -> print_endline ""; handle_save_file Load
  | "2" -> print_endline ""; handle_save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

(* Execute the game. *)
let () = main_menu ()