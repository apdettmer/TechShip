open Printf
open Unix
open Yojson.Basic.Util

type product = {
  name : string
}

type employee = {
  name : string;
  morale : int;
  reputation : int;
}

type investor = { 
  name : string;
  investment : int
}

type company = {
  product : product;
  funding : int;
  reputation : int;
  morale : int;
  employees : employee list;
  investors : investor list;
  date : tm;
}

(** TODO: Make a type for a product. I'm not sure what kinds of fields to 
    include that would impact gameplay, other than a name. -ew424 *)
let new_product name = 
  {name = name}

(** [new_employee] takes a name, perhaps given by the player?, and returns an
    employee with that name and with random morale values ranging from -10 to 10 
*)
let new_employee name = 
  Random.init (int_of_float (Unix.time ()));
  {
    name = name;
    morale = Random.int 20 - 10;
    reputation = Random.int 20 - 10;
  }

let hire_employee name company = let employee = new_employee name in 
  {
    product = company.product;
    funding = company.funding;
    reputation = company.reputation + employee.reputation;
    morale = company.morale + employee.morale;
    employees = employee :: company.employees;
    investors = company.investors;
    date = company.date;
  }

(**[starting_fund start_amount] returns [start_amount] multiplied by a random
   number <10. This creates a different starting funding for every game, making
   the playthrough experience variable. 
   Requires: [start_amount] is > 0, and preferably a large number. *) 
let starting_fund start_amount = 
  Random.init (int_of_float (Unix.time ()));
  start_amount * Random.int 10

let new_company name = {
  product = new_product name;
  funding = (*starting_fund*) 5000;
  reputation = 50;
  morale = 50;
  employees = [];
  investors = [];
  date = {
    tm_sec = 45;
    tm_min = 23;
    tm_hour = 1;
    tm_mday = 2;
    tm_mon = 6;
    tm_year = 2020;
    tm_wday = 2;
    tm_yday = 154;
    tm_isdst = true
  }
}

(* Below are the getters for founding. All very simple.  *)
let product company = 
  company.product

let funding company =
  company.funding

let reputation company =
  company.reputation

let morale company =
  company.morale

let employees company =
  company.employees

let investors company =
  company.investors

let date company =
  company.date

let save_product company =
  sprintf "\t\"product\":{\n\t\t\"name\": \"%s\"\n\t}," company.product.name

let save_funding company =
  sprintf "\t\"funding\": %i," company.funding

let save_reputation company =
  sprintf "\t\"reputation\": %i," company.reputation

let save_morale company =
  sprintf "\t\"morale\": %i," company.morale

let save_employees_helper (employee : employee) =
  sprintf "\t\t{\n\t\t\t\"name\": \"%s\",\n\t\t\t\"morale\": %i,\n\t\t\t\"reputation\": %i\n\t\t}" employee.name employee.morale employee.reputation

let save_employees company =
  sprintf "\t\"employees\": [\n%s\n\t]," (List.rev_map save_employees_helper company.employees |> String.concat ",\n") (*(String.concat ",\n" [(save_employees_helper_test ());(save_employees_helper_test ())])*)

let save_investors_helper (investor : investor) =
  sprintf "\t\t{\n\t\t\t\"name\": \"%s\",\n\t\t\t\"investment\": %i\n\t\t}" investor.name investor.investment

let save_investors company =
  sprintf "\t\"investors\": [\n%s\n\t]," (List.rev_map save_investors_helper company.investors |> String.concat ",\n")

let save_date company =
  sprintf "\t\"date\": {\n\t\t\"second\": %i,\n\t\t\"minute\": %i,\n\t\t\"hour\": %i,\n\t\t\"month day\": %i,\n\t\t\"month\": %i,\n\t\t\"year\": %i,\n\t\t\"week day\": %i,\n\t\t\"year day\": %i,\n\t\t\"daylight saving\": %b\n\t}" company.date.tm_sec company.date.tm_min company.date.tm_hour company.date.tm_mday company.date.tm_mon company.date.tm_year company.date.tm_wday company.date.tm_yday company.date.tm_isdst

let save company =
  let file_name = company.product.name in
  let save_file = String.concat "" [file_name; ".json"] in
  let out_chn = open_out save_file in
  let data = String.concat "\n" ["{"; save_product company; save_funding company; save_reputation company; save_morale company; save_employees company; save_investors company; save_date company; "}"] in
  fprintf out_chn "%s" data

(* let load_product =

   let load json = {
    product = json |> member "product" 
   } *)

(** I'm not sure if we should display the other stats. Maybe we print the 
    names of investors and amount invested? -ew424 *)
let print_stats company = 
  printf "Funding: %i\n" (funding company);
  printf "Reputation: %i\n" (reputation company);
  printf "Morale: %i\n" (morale company);
  printf "Number of employees: %i\n" (List.length (employees company))

(** TODO: Input an event or response to an event (for ex. a record with data on
    the stat changed to be made) and apply that response to the company,
    producing a new one *)
let update_company event company =
  failwith "unimplemented"