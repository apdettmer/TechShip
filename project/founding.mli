(* design the company in the growth phase *)
type employee

type product

type investor

type company

(** [new_blank_company] creates a new company with the default starting values,
    which are currently hardcoded in but can be changed if needed. Additionally
    the input may be changed from string to product if we decided to make the 
    product more descriptive. *)
val new_company : string -> company

(** [hire_employee] takes a string of an employee's name and then randomly
    generates values for morale and reputation, adds them to the employee
    list, and produces a new company with updated stats. *)
val hire_employee : string -> int -> company -> company


(** [name investor] gives the name of [investor]*)
val name : investor -> string

(**[emp_name employee] gives the name of [employee] *)
val emp_name : employee -> string

(** [emp_morale employee] gives the morale of [employee]*)
val emp_morale : employee -> int 

(** [emp_reputation employee] gives the reputation of [employee]*)
val emp_reputation : employee -> int 

(** [investment investor] gives the amount [investor] contributes to a company
    if allowed*)
val investment : investor -> int

(** [employee_name emp] is the name of given employee [emp] *)
val employee_name : employee -> string

(**[employee_list name n acc] is the updated list of employees with [n] new 
   employees with name [n] added to it. *)
val employee_list : string -> int -> employee list -> employee list

(**[new_employee name] takes in a name and creates an employee with random
   morale and reputation stats *)
val new_employee : string -> employee

(**[new_random_employee ()] generates an [employee] with a random name from
   the name bank and random morale and reputation attributes. *)
val new_random_employee : unit -> employee

(**[custom_employee name morale rep] creates an [employee] with the same 
   attributes as the arguments provided *)
val custom_employee : string -> int -> int -> employee

(**[custom_investor name invest] creates an [investor] with the same 
   attributes as the arguments provided *)
val custom_investor : string -> int -> investor

(**[morale_employees empy_list] is the total change in morale caused by the 
   hiring of employee list [emp_list] *)
val morale_employees : employee list -> int

(**[rep_employees emp_list] is the total change in reputation caused by the 
   list of employees [empy_list]*)
val rep_employees : employee list -> int

(** [product company] is the product of the company. *)
val product : company -> product

(** [funding company] is the current funding the company has. *)
val funding : company -> int

(** [set_event company category id] updates the event field of [comapny]
    to [category], [id]*)
val set_event : company -> string -> int -> company

(** [employees company] is an [employee] list of the current employees. *)
val employees : company -> employee list

(** [string_of_product product] is the name of [product]. *)
val string_of_product : product -> string

(** [reputation company] is the reputation of the company. *)
val reputation : company -> int

(** [morale company] is the morale of the company. *)
val morale : company -> int

(** [date company] is the current date. *)
val date : company -> Unix.tm

(** [event company] is the id and category of the event in [company]*)
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

val update_category : company -> string -> int -> company

(**[check_lost_phase1 company] returns true if the company has failed. A company
   fails when one of its attributes (funding, morale, reputation) is <= 0. If 
   true, it also prints out the appropriate end game message the to the player.
   If false, prints nothing.  *)
val check_lost_phase1 : company -> bool

(** [add_employee emp company] gives [company] with [emp] appended to the list
    of employees*)
val add_employee : employee -> company -> company

(** [add_investor inv company] gives [company] with [inv] appended to the list
    of investors*)
val add_investor : investor -> company -> company