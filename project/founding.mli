(** [employee] represents an employee of the company. *)
type employee

(** [investor] represents an investor of the company. *)
type investor

(** [company] represents the unfounded company in the Founding phase. *)
type company

(** [new_blank_company] creates a new company with the default starting 
    values. *)
val new_company : string -> company

(** [hire_employee] takes a string of an employee's name and then randomly
    generates values for morale and reputation, adds them to the employee
    list, and produces a new company with the employee as a component and with 
    updated morale and reputation. *)
val hire_employee : string -> int -> company -> company

(** [name investor] gives the name of [investor]. *)
val name : investor -> string

(** [emp_name employee] gives the name of [employee]. *)
val emp_name : employee -> string

(** [emp_morale employee] gives the morale of [employee]. *)
val emp_morale : employee -> int 

(** [emp_reputation employee] gives the reputation of [employee]. *)
val emp_reputation : employee -> int 

(** [investment investor] gives the amount [investor] contributes to a company
    if allowed. *)
val investment : investor -> int

(** [employee_name emp] is the name of given employee [emp]. *)
val employee_name : employee -> string

(** [employee_list name n acc] is the updated list of employees with [n] new 
    employees with [name] added to it. *)
val employee_list : string -> int -> employee list -> employee list

(** [new_employee name] takes in a name and creates an employee with random
    morale and reputation stats. *)
val new_employee : string -> employee

(** [new_random_employee ()] generates an [employee] with a random name from
    the name bank and random morale and reputation attributes. *)
val new_random_employee : unit -> employee

(** [custom_employee name morale rep] creates an employee with the same 
    attributes as the arguments provided. *)
val custom_employee : string -> int -> int -> employee

(** [custom_investor name invest] creates an investor with the same 
    attributes as the arguments provided. *)
val custom_investor : string -> int -> investor

(** [morale_employees emp_list] is the total change in morale caused by the 
    hiring of employee list [emp_list]. *)
val morale_employees : employee list -> int

(** [rep_employees emp_list] is the total change in reputation caused by the 
    list of employees [emp_list]. *)
val rep_employees : employee list -> int

(** [product company] is the product of the company. *)
val product : company -> string

(** [funding company] is the current funding the company has. *)
val funding : company -> int

(** [set_event company category id] updates the event field of [company] to 
    [category], [id]. *)
val set_event : company -> string -> int -> company

(** [employees company] is an [employee] list of the current employees. *)
val employees : company -> employee list

(** [reputation company] is the reputation of the company. *)
val reputation : company -> int

(** [morale company] is the morale of the company. *)
val morale : company -> int

(** [date company] is the current date. *)
val date : company -> Unix.tm

(** [event company] is the id and category of the event in [company]. *)
val event : company -> string * int

(** [investors company] is an [investor] list of investors in the company. *)
val investors : company -> investor list

(** [save] writes to or creates a JSON file with the save data of the 
    company. *)
val save : company -> unit

(** [load json] is the company represented in json. *)
val load : Yojson.Basic.t -> company

(** [print_stats] prints out the current stats of the company to the player. *)
val display_status : company -> unit

(** [update_category company cat v] produces a new company with the field 
    provided by [cat] incremented by [v]. *)
val update_category : company -> string -> int -> company

(** [check_lost_phase1 company] is whether the company has failed. A company 
    fails when one of its attributes (funding, morale, reputation) is at most 
    0. If the company has failed, the appropriate end game message is printed 
    to the player. If the company has not failed, nothing is printed. *)
val check_lost_phase1 : company -> bool

(** [add_employee emp company] gives [company] with [emp] added to the list
    of employees. *)
val add_employee : employee -> company -> company

(** [add_investor inv company] gives [company] with [inv] added to the list
    of investors. *)
val add_investor : investor -> company -> company