(* TODO: Create the module for the company in the growth phase *)


type employee = {
  name : string;
  morale : int;
}

type investor = { 
  name : string;
  funds_invested : int
}

type product = string

type company = {
  product : product;
  funding : int;
  employees : employee list;
  reputation : int;
  morale : int;
  investors : investor list;

}

(** TODO: Make a type for a product. I'm not sure what kinds of fields to 
    include that would impact gameplay, other than a name. -ew424 *)
let new_product name = 
  name

(**[starting_fund start_amount] returns [start_amount] multiplied by a random
   number <10. This creates a different starting funding for every game, making
   the playthrough experience variable. 
   Requires: [start_amount] is > 0, and preferably a large number. *) 
let starting_fund start_amount = 
  Random.init (int_of_float (Unix.time ()));
  start_amount * Random.int 10

let new_company name = {
  product = new_product name;
  funding = starting_fund 1000000;
  employees = [];
  reputation = 50;
  morale = 50;
  investors = []
}

(* Below are the getters for founding. All very simple.  *)
let product company = 
  company.product

let funding company =
  company.funding

let employees company =
  company.employees

let reputation company =
  company.reputation

let morale company =
  company.morale

let investors company =
  company.investors

(** I'm not sure if we should display the other stats. Maybe we print the 
    names of investors and amount invested? -ew424 *)
let print_stats company = 
  Printf.printf "Funding: %i\n" (funding company);
  Printf.printf "Reputation: %i\n" (reputation company);
  Printf.printf "Morale: %i\n" (morale company);
  Printf.printf "Number of employees: %i\n" (List.length (employees company))

(** TODO: Input an event or response to an event (for ex. a record with data on
    the stat changed to be made) and apply that response to the company,
    producing a new one *)
let update_company event company =
  failwith "unimplemented"

