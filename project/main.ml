open Founding
open Growth
open Event

(**[play] is the repl loop that takes player input and determines actions
   in the game. [player_file] is a JSON file that is a save file. *)
let rec play file_name = 
  print_endline "Let the games begin…"

let create_new_game () =
  print_endline "> be you";
  print_endline "> recent graduate of Cornell Engineering";
  print_endline "> be broke and jobless";
  print_endline "> your parents lent you $5,000 and said \"get a life\"";
  print_endline "> you have an idea for the Next Big Thing™:";
  print_string "> ";
  read_line () |> new_company |> save |> play

let json_extension file =
  let re = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward re file 0);
    true
  with Not_found -> false

let load_save_file_helper () =
  Sys.readdir "." |> Array.to_list |> List.filter (fun x -> json_extension x) |>
  List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) |>
  List.sort String.compare |> List.iter print_endline

let load_save_file () =
  print_endline "Select a game:";
  load_save_file_helper ();
  print_string "> ";
  String.concat "" [read_line (); ".json"] |> play

let rec main_menu_helper () =
  print_endline "Invalid entry.";
  print_endline "";
  print_endline "[1] Create new game.";
  print_endline "[2] Load game.";
  print_endline "[3] Quit.";
  print_string "> ";
  match read_line () with
  | "1" -> print_endline ""; create_new_game ()
  | "2" -> print_endline ""; load_save_file ()
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

(** [main ()] prompts for the game to play, then starts it. *)
let main_menu () =
  print_endline "";
  print_endline "";
  print_endline "TECHSHIP";
  print_endline "";
  print_endline "[1] Create new game.";
  print_endline "[2] Load game.";
  print_endline "[3] Quit.";
  print_string "> ";
  match read_line () with
  | "1" -> print_endline ""; create_new_game ()
  | "2" -> print_endline ""; load_save_file ()
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

(* Execute the game. *)
let () = main_menu ()