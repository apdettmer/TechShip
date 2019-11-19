open Yojson.Basic.Util

open Founding
open Growth
open Event


let rec display_responses count = function
  | [] -> ()
  | h :: t -> Stdlib.print_string ("[" ^ (string_of_int count) ^ "] ");
    Stdlib.print_endline (response_description h); display_responses (count + 1) t

(**[display_event company] generates a random event of type [e], prints out
   its description and returns it. *)
let display_event company = 
  let e = company |> Event.random_category |> Event.random_event in 
  Stdlib.print_endline (description e); Stdlib.print_endline "";
  e

(**[play] is the repl loop that takes player input and determines actions
   in the game. [player_file] is a JSON file that is a save file. *)
let rec play company =
  display_status company;
  let event = display_event company in display_responses 0 (responses event);
  Stdlib.print_endline "\nHow will you respond? "




let create_new_save () =
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

let json_extension file =
  let extension = Str.regexp_string ".json" in
  try 
    ignore (Str.search_forward extension file 0);
    true
  with Not_found -> false

type load_or_delete = Load | Delete

let save_file_helper load_or_delete file_name =
  match load_or_delete with
  | Load -> Yojson.Basic.from_file file_name |> load |> play
  | Delete -> Sys.remove file_name

let list_files () =
  Sys.readdir "." |> Array.to_list |> List.filter (fun x -> json_extension x) |> List.map (fun x -> Str.global_replace (Str.regexp_string ".json") "" x) |> List.sort String.compare

let rec find_file load_or_delete save_files input=
  if List.mem input save_files
  then String.concat "" [input; ".json"] |> save_file_helper load_or_delete
  else (print_endline "Invalid entry.";
        print_endline "";
        save_file load_or_delete)

and

  save_file load_or_delete =
  print_endline "Select a save:";
  let save_files = list_files () in
  List.iter print_endline save_files;
  print_string ">";
  read_line () |> find_file load_or_delete save_files

let rec main_menu_helper () =
  print_endline "Invalid entry.";
  print_endline "";
  print_endline "[0] Create new save.";
  print_endline "[1] Load save.";
  print_endline "[2] Delete save.";
  print_endline "[3] Quit.";
  print_string ">";
  match read_line () with
  | "0" -> print_endline ""; create_new_save ()
  | "1" -> print_endline ""; save_file Load
  | "2" -> print_endline ""; save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()

(** [main ()] prompts for the game to play, then starts it. *)
let main_menu () =
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
  | "1" -> print_endline ""; save_file Load
  | "2" -> print_endline ""; save_file Delete
  | "3" -> print_endline ""; exit 0
  | _ -> main_menu_helper ()


(* Execute the game. *)
let () = main_menu ()