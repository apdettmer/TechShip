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
type f_response = { 
  description : string;
  effects : (string * int option) list
}

let f_effects f_resp = 
  f_resp.effects

let f_description f_resp =
  f_resp.description

let new_f_response desc effects = {
  description = desc;
  effects = effects
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
  | _ -> failwith "ERROROR1"
