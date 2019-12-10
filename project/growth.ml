open Unix

type founded = {
  product : Founding.product;
  market_cap : int;
  reputation : int;
  morale : int;
  teams : Founding.employee list list;
  investors: Founding.investor list;
  date : tm;
  marketing: int;
  management: int;
  (* event : string * int *)
}

(* Currently defined exactly as the one in event - but I feel like its better to
   create a new type for this file / new type of company -ew424 *)
type f_response = Event.response

let f_effects f_resp = 
  Event.effects f_resp

let f_description f_resp =
  Event.res_desc f_resp

let new_f_response desc effects = 
  Event.new_response desc effects

let found company = 
  { 
    product = Founding.product company;
    market_cap = Founding.funding company;
    reputation = Founding.reputation company;
    morale = Founding.morale company;
    teams = [];
    investors = Founding.investors company;
    date = Founding.date company;
    marketing = 50;
    management = 100;
    (* event = "easter egg", 0 *)
  }

let product founded = 
  founded.product

let market_cap founded = 
  founded.market_cap

let reputation founded = 
  founded.reputation

let morale founded =
  founded.morale

let teams founded = 
  founded.teams

let investors founded =
  founded.investors

let date founded =
  founded.date

let marketing founded = 
  founded.marketing

let management founded = 
  founded.management

(* let event founded = 
   founded.event *)


let update_investors current_investor_list = 
  failwith "Not sure how we wanna go about implementing this"

let update_teams current_team_list =
  failwith "Again, not sure how we should do this"

let update_cat name v company= 
  match name with 
  | "market_cap" -> { 
      product = product company;
      market_cap = market_cap company + v;
      reputation = reputation company;
      morale = morale company;
      teams = teams company;
      investors = investors company;
      date = date company;
      marketing = marketing company;
      management = management company;
    }
  | "reputation" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company + v;
      morale = morale company;
      teams = teams company;
      investors = investors company;
      date = date company;
      marketing = marketing company;
      management = management company;
    }
  | "morale" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company;
      morale = morale company + v;
      teams = teams company;
      investors = investors company;
      date = date company;
      marketing = marketing company;
      management = management company;
    }
  | "investors" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company;
      morale = morale company;
      teams = teams company;
      investors = update_investors (investors company);
      date = date company;
      marketing = marketing company;
      management = management company;
    }
  | "marketing" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company;
      morale = morale company;
      teams = teams company;
      investors = investors company;
      date = date company;
      marketing = marketing company + v;
      management = management company;
    }
  | "management" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company;
      morale = morale company;
      teams = teams company;
      investors = investors company;
      date = date company;
      marketing = marketing company;
      management = management company + v;
    }
  | "teams" -> { 
      product = product company;
      market_cap = market_cap company;
      reputation = reputation company;
      morale = morale company;
      teams = update_teams (teams company);
      investors = investors company;
      date = date company;
      marketing = marketing company;
      management = management company;
    }
  | _ -> failwith "ERROROR2"

let rec update_founded founded f_resp =
  match f_effects (f_resp) with 
  | [] -> founded
  | (s, Some(v)) :: t -> 
    update_founded (update_cat s v founded) 
      (new_f_response (f_description f_resp) t)
  | (s, None) :: t -> 
    update_founded founded (new_f_response (f_description f_resp) t)


let print_founded founded = 
  Stdlib.print_endline "";
  Stdlib.print_endline 
    ("Market Capitalization: " ^ string_of_int (market_cap founded));
  Stdlib.print_endline 
    ("Reputation: " ^ string_of_int (reputation founded));
  Stdlib.print_endline 
    ("Morale: " ^ string_of_int (morale founded));
  Stdlib.print_endline 
    ("Number of investors: " ^ string_of_int (List.length (investors founded)));
  Stdlib.print_endline 
    ("Number of teams: " ^ string_of_int (List.length (teams founded)));
  Stdlib.print_endline 
    ("Marketing : " ^ string_of_int (marketing founded));
  Stdlib.print_endline 
    ("Management: " ^ string_of_int (management founded));
  Stdlib.print_endline ""

let print_found_message msg = 
  Stdlib.print_string "\n Your efforts in the founding phase have paid off. "; 
  Stdlib.print_string ("Now it is time to take ");
  ANSITerminal.(print_string [green] msg); 
  Stdlib.print_string (" to the next level. \n");
  Stdlib.print_string "You have entered the growth phase. Some stats, such as";
  Stdlib.print_string " Morale and Reputation are the same, while you gained new ";
  Stdlib.print_string "stats such as Marketing and Managment. Good luck. \n \n"

let print_founded_change change field =
  match change with
  | 0 -> ()
  | v -> Stdlib.print_string ("(" ^ field ^ "): ");
    if v >= 0 then ANSITerminal.(print_string [green] ("+" ^ string_of_int v))
    else ANSITerminal.(print_string [red] (string_of_int v));
    Stdlib.print_endline "" 

let print_updates prev_found new_founded = 
  print_founded_change ((market_cap new_founded) - (market_cap prev_found)) ("market_cap");
  print_founded_change ((reputation new_founded)- (reputation prev_found)) ("reputation");
  print_founded_change ((morale new_founded)- (morale prev_found)) ("morale");
  (* print_founded_change ((investor new_founded)- ( prev_found)) ("investors"); *)
  (* print_founded_change (teams prev_found) (teams new_founded)  ("teams"); *)
  print_founded_change ((marketing new_founded) - (marketing prev_found)) ("marketing");
  print_founded_change ((management new_founded) - (management prev_found)) ("management");
  Stdlib.print_string "\n"

let check_lost_market_cap v = 
  match v with
  | _ when v > 0 -> true
  | _ -> Stdlib.print_endline "";
    Stdlib.print_string("Your company has run out money and subsequently "^
                        "failed. The public just didn't seem to believe " ^
                        "in you. Perhaps you could " ^ 
                        "have made wiser financial decisions. \n \n");false

let check_lost_reputation v = 
  match v with
  | _ when v > 0 -> true
  | _ -> Stdlib.print_endline "";
    Stdlib.print_string("The public perception of you as founder and CEO "^
                        "have hit new lows. You're one of the most hated" ^
                        " people in the country, and are forced to shut " ^
                        "down the company. Try to be more likable next" ^
                        " time. \n \n"); false

let check_lost_morale v = 
  match v with
  | _ when v > 0 -> true
  | _ -> Stdlib.print_endline "";
    Stdlib.print_string("Company morale is at rock bottom. Nobody wants " ^ 
                        "to work for you and many of your employees have" ^
                        " quit. With no employees, the company cannot no "^ 
                        "longer operate.\n \n"); false

let check_lost_marketing v = 
  match v with
  | _ when v > 0 -> true
  | _ -> Stdlib.print_endline "";
    Stdlib.print_string("The marketing department has failed entirely." ^
                        " Nobody anywhere even knows your product or " ^
                        "who you are. No customers means no revenue and " ^
                        "a failed company.\n \n"); false

let check_lost_management v = 
  match v with
  | _ when v > 0 -> true
  | _ -> Stdlib.print_endline "";
    Stdlib.print_string("Your upper management has gone from bad to worse"^
                        ". Repeated poor decisions have caused multiple " ^
                        "locations to fail. The losses are too great to " ^
                        "overcome. The company has failed. In the future" ^
                        ", hire more Cornell alums. \n \n"); false

let check_won_lost founded = 
  check_lost_market_cap (market_cap founded) && 
  check_lost_reputation (reputation founded) && 
  check_lost_morale (morale founded) &&
  check_lost_marketing (marketing founded) &&
  check_lost_management (management founded) 