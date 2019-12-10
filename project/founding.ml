open Printf
open Unix
open Yojson.Basic.Util

type self = {
  name : string;
  age : tm;
  health : int;
}

type product = {
  name : string
}

type investor = { 
  name : string;
  investment : int
}

type employee = {
  name : string;
  morale : int;
  reputation : int;
}

type company = {
  product : product;
  funding : int;
  reputation : int;
  morale : int;
  employees : employee list;
  investors : investor list;
  date : tm;
  event: string * int;
}

let new_product name = {
  name = name
}

(** [new_employee] takes a name, perhaps given by the player?, and returns an
    employee with that name and with random morale values ranging from -5 to 5 
*)
let new_employee name = 
  Random.init (int_of_float (Unix.time ()));
  {
    name = name;
    morale = Random.int 10 - 5;
    reputation = Random.int 10 - 5;
  }

let rand_name = function
  | 0 -> "Jeff"
  | 1 -> "Ron"
  | 2 -> "Steve"
  | 3 -> "Bill"
  | 4 -> "Tim"
  | 5 -> "Dick" 
  | 6 -> "Ken"
  | 7 -> "Ben"
  | 8 -> "James"
  | 9 -> "Arthur"
  | _ -> "Naming Error"

let new_random_employee () = 
  Random.init (int_of_float (Unix.time ()));
  {
    name = rand_name (Random.int 10);
    morale = Random.int 20 - 10;
    reputation = Random.int 20 - 10;
  }

let custom_employee name morale rep = {
  name = name;
  morale = morale;
  reputation = rep;
}

let string_of_product (product : product) = 
  product.name

let rec employee_list name n acc = 
  match n with 
  | 0 -> acc
  | _ ->  if n > 0 
    then employee_list name (n-1) (new_random_employee () :: acc)
    else []

let rec rep_employees ( emp_list : employee list ) = 
  let rep_e (employee : employee)  = employee.reputation in
  match emp_list with
  | [] -> 0
  | h :: t -> rep_e h + rep_employees t

let rec morale_employees (emp_list : employee list) = 
  let mor_e (employee : employee)  = employee.morale in
  match emp_list with
  | [] -> 0
  | h :: t -> mor_e h + morale_employees t

(**[hire_employee name n company] hires [n] new employees for the company. The
   company is updated with a new employee list and new morale and reputation 
   depending on the reputation of the employees hired. *)
let hire_employee name n company = 
  let employees = employee_list name n [] in 
  {
    product = company.product;
    funding = company.funding;
    reputation = company.reputation + rep_employees employees;
    morale = company.morale + morale_employees employees;
    employees = company.employees @ employees;
    investors = company.investors;
    date = company.date;
    event = company.event;
  }

let add_employee (emp:employee) company = {
  product = company.product;
  funding = company.funding;
  reputation = company.reputation + emp.reputation;
  morale = company.morale + emp.morale;
  employees = emp :: company.employees;
  investors = company.investors;
  date = company.date;
  event =company.event
}

let add_investor inv company = {
  product = company.product;
  funding = company.funding;
  reputation = company.reputation;
  morale = company.morale;
  employees = company.employees;
  investors = inv :: company.investors;
  date = company.date;
  event =company.event
}

let employee_name emp = 
  emp.name

let print_employee_info emp = 
  Stdlib.print_string 
    (" { Name: " ^ emp.name ^ " | Morale: " ^ string_of_int emp.morale 
     ^ " | Reputation: " ^ string_of_int emp.reputation ^ " } ");
  Stdlib.print_endline ""; ()  

let rec view_employees emp_list =
  match emp_list with 
  | [] -> ()
  | h :: t -> print_employee_info h; view_employees t


let new_company name = {
  product = new_product name;
  funding = 5000;
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
  };
  event = "sample", 0;
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

let event company =
  company.event

let set_event company category id = {
  product = company.product;
  funding = company.funding;
  reputation = company.reputation;
  morale = company.morale; 
  employees = company.employees;
  investors = company.investors;
  date = company.date;
  event = category, id
}

let save_product company =
  sprintf "\t\"product\":{
\t\t\"name\": \"%s\"
\t}," company.product.name

let save_funding company =
  sprintf "\t\"funding\": %i," company.funding

let save_reputation company =
  sprintf "\t\"reputation\": %i," company.reputation

let save_morale company =
  sprintf "\t\"morale\": %i," company.morale

let save_employees_helper (employee : employee) =
  sprintf "\t\t{
\t\t\t\"name\": \"%s\",
\t\t\t\"morale\": %i,
\t\t\t\"reputation\": %i
\t\t}" employee.name employee.morale employee.reputation

let save_employees company =
  sprintf "\t\"employees\": [
%s
\t]," (List.rev_map save_employees_helper company.employees 
       |> String.concat ",\n")

let save_investors_helper (investor : investor) =
  sprintf "\t\t{
\t\t\t\"name\": \"%s\",
\t\t\t\"investment\": %i
\t\t}" investor.name investor.investment

let save_investors company =
  sprintf "\t\"investors\": [
%s
\t]," (List.rev_map save_investors_helper company.investors 
       |> String.concat ",\n")

let save_date company =
  sprintf "\t\"date\": {
\t\t\"second\": %i,
\t\t\"minute\": %i,
\t\t\"hour\": %i,
\t\t\"month day\": %i,
\t\t\"month\": %i,
\t\t\"year\": %i,
\t\t\"week day\": %i,
\t\t\"year day\": %i,
\t\t\"daylight saving\": %b
\t}," company.date.tm_sec company.date.tm_min company.date.tm_hour 
    company.date.tm_mday company.date.tm_mon company.date.tm_year 
    company.date.tm_wday company.date.tm_yday company.date.tm_isdst

let save_event company =
  sprintf "\t\"event\": {
  \t\t\"category\": \"%s\",
  \t\t\"id\": %i
  }" (fst(company.event)) (snd(company.event))

let save company =
  let file_name = company.product.name in
  let save_file = String.concat "" [file_name; ".json"] in
  let out_chn = open_out save_file in
  let data = String.concat "\n" [
      "{"; "\t\"phase\": 1,";
      save_product company; 
      save_funding company; 
      save_reputation company; 
      save_morale company; 
      save_employees company; 
      save_investors company; 
      save_date company; 
      save_event company; "}"] in
  fprintf out_chn "%s" data;
  flush out_chn

let load_product json_product = {
  name = json_product |> member "name" |> to_string
}

let load_employee json_employee = {
  name = json_employee |> member "name" |> to_string;
  morale = json_employee |> member "morale" |> to_int;
  reputation = json_employee |> member "reputation" |> to_int;
}

let load_investor json_investor = {
  name = json_investor |> member "name" |> to_string;
  investment = json_investor |> member "investment" |> to_int;
}

let custom_investor name invest = {
  name = name;
  investment = invest;
}

let load_date json_date = {
  tm_sec = json_date |> member "second" |> to_int;
  tm_min = json_date |> member "minute" |> to_int;
  tm_hour = json_date |> member "hour" |> to_int;
  tm_mday = json_date |> member "month day" |> to_int;
  tm_mon = json_date |> member "month" |> to_int;
  tm_year = json_date |> member "year" |> to_int;
  tm_wday = json_date |> member "week day" |> to_int;
  tm_yday = json_date |> member "year day" |> to_int;
  tm_isdst = json_date |> member "daylight saving" |> to_bool;
}

let load json = {
  product = json |> member "product" |> load_product;
  funding = json |> member "funding" |> to_int;
  reputation = json |> member "reputation" |> to_int;
  morale = json |> member "morale" |> to_int;
  employees = json |> member "employees" |> to_list |> List.map load_employee;
  investors = json |> member "investors" |> to_list |> List.map load_investor;
  date = json |> member "date" |> load_date;
  event = 
    let cat = json |> member "event" |> member "category" |> to_string in
    let id = json |> member "event" |> member "id" |> to_int in 
    cat, id;
}

let display_status company =
  printf "Funding: %i\n" (funding company);
  printf "Reputation: %i\n" (reputation company);
  printf "Morale: %i\n" (morale company);
  printf "Number of employees: %i\n\n" (List.length (employees company))

let update_category company cat v = 
  match cat with 
  | "funding" -> {
      product = company.product;
      funding = company.funding + v;
      reputation = company.reputation;
      morale = company.morale; 
      employees = company.employees;
      investors = company.investors;
      date = company.date;
      event = company.event
    }
  | "reputation" -> {
      product = company.product;
      funding = company.funding;
      reputation = company.reputation + v;
      morale = company.morale;
      employees = company.employees;
      investors = company.investors;
      date = company.date;
      event = company.event
    }
  | "morale" -> {
      product = company.product;
      funding = company.funding;
      reputation = company.reputation;
      morale = company.morale + v;
      employees = company.employees;
      investors = company.investors;
      date = company.date;
      event = company.event
    }
  | "employee" -> hire_employee "John" v company

  | _ -> failwith "Unimplemented"

let fundL = "Uh-oh. You ran out of funding before your company could really " ^
            "get off the ground. With no money, and no real success thus far," ^
            " you can no longer continue operations. You should've have paid" ^
            " better attention in Finance class. Imagine what could have been."

let check_lost_funding v = 
  match v with
  | _ when v <= 0 -> Stdlib.print_endline fundL; 
    Stdlib.print_endline ""; true
  | _ -> false

let repL = "The repuation of your company has fallen dangerously low due to " ^
           "your questionable decision making. No one trusts you, and invest" ^
           "ors are pulling their money. Public perception is too low for yo" ^
           "ur company to have any sort of success. The company must be shut" ^
           "down. If only you had been more ethical. "

let check_lost_reputation v = 
  match v with
  | _ when v <= 0 -> Stdlib.print_endline repL;
    Stdlib.print_endline ""; true
  | _ -> false

let moraleL = "The morale inside your company is non existent. Nobody, not e" ^
              "ven you, believes the product can succeed. All motivation has" ^
              " disappeared, kinda like that time you took CS 3110 in colleg" ^
              "e. With no morale, your company failed at an early stage."

let check_lost_morale v = 
  match v with
  | _ when v <= 0 -> Stdlib.print_endline moraleL; 
    Stdlib.print_endline ""; true
  | _ -> false


let check_lost_phase1 company = 
  check_lost_funding (funding company) ||
  check_lost_reputation (reputation company) ||
  check_lost_morale (morale company)