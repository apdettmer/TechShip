(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to TechShip.\n");
  print_endline "Would you like to start a new game or load a save?\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> ()

(* Execute the game engine. *)
let () = main ()