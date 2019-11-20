(* design the company in the growth phase *)
type company

type employee

type product

type investor

(** [new_blank_company] creates a new company with the default starting values,
    which are currently hardcoded in but can be changed if needed. Additionally
    the input may be changed from string to product if we decided to make the 
    product more descriptive. *)
val new_company : string -> company

(** [hire_employee] takes a string of an employee's name and then randomly
    generates values for morale and reputation, adds them to the employee
    list, and produces a new company with updated stats. *)
val hire_employee : string -> company -> company

(** [product] is the product of the company. *)
val product : company -> product

(** [funding] is the current funding the company has. *)
val funding : company -> int

(** [employees] is an [employee] list of the current employees. *)
val employees : company -> employee list

(** [reputation] is the reputation of the company. *)
val reputation : company -> int

(** [morale] is the morale of the company. *)
val morale : company -> int

(** [date] is the current date. *)
val date : company -> Unix.tm

(** [investors] is an [investor] list of investors in the company. *)
val investors : company -> investor list

(** [save] writes to or creates a JSON file with the save data of the 
    company. *)
val save : company -> unit

(** [load json] is the company represented in json. *)
val load : Yojson.Basic.t -> company

(** [print_stats] prints out the current stats of the company to the player. *)
val display_status : company -> unit