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
  management: int

}

let found company = 
  { 
    product = Founding.product company;
    market_cap = Founding.funding company;
    reputation = Founding.reputation company;
    morale = Founding.morale company;
    teams = [];
    investors = Founding.investors company;
    date = Founding.date company;
    marketing = 100;
    management = 50
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
    ("Management: " ^ string_of_int (management founded))

(* this is my attempt at making a repl loop for growth

   (**[display_event company] generates a random event of type [e], prints out
     its description and returns it. *)
   let display_event company =
   let event = fill_event_description 
      ((*company |> random_category*) "demo" |> Event.random_event) 
      (select_some_word ()) 20 in
   print_endline (description event);
   event


   let rec growth_play founded = 
   let event = display_event_founded () in
   let responses = display_responses event in 
   let result = handle_response display_responses in
   let updated_f = update_founded updated_f in
   growth_play updated_f
*)