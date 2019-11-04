exception Empty

(**[play] is the repl loop that takes player input and determines actions
   in the game. [player_file] is a JSON file that is a save file. *)
let play player_file = 
  failwith "."



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to TechShip.\n");
  print_endline "Would you like to start a new game or load a save?\n";
  print_string  "> ";
  match read_line () with
  | exception Empty -> ()
  | "1" -> play 
  | _ -> play
