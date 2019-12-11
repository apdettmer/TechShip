open Event
open Founding
open Growth
open OUnit2
open Yojson.Basic.Util

(**[TEST PLAN]: Overall, we used OUnit to test certain features where we could
     and manually tested the features OUnit couldn't test.

     For the functions from the different modules that produced values, we used
     OUnit tests (written below) to ensure they are correct. We created multiple
     test suites to reflect the certain module they test. We had one for 
     event.ml, founding.ml and growth.ml, the three main modules that comprise
     the game. For testing events, we pulled a sample event for our JSON and 
     then used assert_equal to make sure the individual fields, such as name, 
     response list and attributes, were correct. For testing founding, the type
     of the company in the first phase, we created a sample company and tested
     the getters to confirm all the default values were correct. Then, we created
     custom responses identical to the responses the player would produce in game,
     and then tested the updated company, using getters on the individual fields, 
     to confirm the updated values were correct. We did this for responses that 
     covered every field, thus proving that the company is always properly 
     updated. We followed this same format to test the hiring of employees. For 
     growth.ml, we had the same pattern. We first created a founded object, the 
     type used for phase 2, and then tested the individual fields with the getters
     to ensure the conversion from phase 1 to phase 2 was correct. Then, we again
     created custom responses to events and tested the update_company feature,
     again testing all the individual fields with unique tests, so that every 
     type of response was properly handled. We also tested the win/lose functions
     on cases that returned a bool and printed nothing. On top of all this,
     as written below, we did significant manual testing by playing the game where
     we could have caught other bugs not covered by the OUnit test. Thus, we know
     the game plays as intended.

     For many of the functions used in main.ml as part of the two repl loops, 
     and the other functions that printed messages to the player, we were unable 
     to use OUnit and instead tested manually through gameplay. Those functions
     returned unit, and did not produce new values for which we could use OUnit
     to test. So, we simply played the game and observed the messages printed. 
     To test the function that displays the company's change in stats, we ensured
     that it worked with all events for all categories. We tested the functions
     that display the company's status by simply selecting that option in game. 
     For the functions that print unique messages at for winning or losing the 
     game, we played the game, with limited events, and ensured that each 
     scenario occurred and the correct message printed. Essentially, for the 
     printing functions, we manually played the game and ensured every possible
     branch would be triggered when it was supposed to and subsequently printed 
     the correct message. Therefore, we are confident all the printed functions
     perform as they are intended to.

*)


let e1 = event_of "sample" 1 "data/events.json"

let r1 = match responses e1 with 
  | h::t -> h 
  | _ -> failwith "Sample esponse not found"

let e2 = event_of "investor" 0 "data/events.json"

let inv_event = constructor_event_of "con_investor" 1
let e_env_event = fst inv_event
let inv_of_event = snd inv_event

let emp_event = constructor_event_of "con_employee" 0
let emp_of_event = snd emp_event

let comp_sample = new_company "sample comp"
let up_comp_sample = update_company r1 comp_sample

let fun_prog = 
  List.nth( Yojson.Basic.from_file "data/wordbank.json"
            |> member "words" 
            |> to_list) 1 |> to_string

let make_event_test
    (name : string)
    (expected_output : 'b)
    (input : 'a)
    (f : 'a -> 'b) : test =
  name >:: (fun _ ->
      assert_equal expected_output (f input))

let random_event_tester category = 
  try ignore(random_event "data/events.json" category);
    true
  with InvalidEventCategory _ -> false

let check_nonempty str =
  str <> ""


let event_tests = [
  make_event_test "category of sample" "sample" e1 Event.category;
  make_event_test "description of sample" "sample string_val int_val" 
    e1 Event.description;
  make_event_test "affected stats list size of sample" 
    2 (Event.affected_stats e1) List.length;
  make_event_test "affected stats list size of investor event e2" 2 
    (Event.affected_stats e2) List.length;
  make_event_test "Response list is 3 elements long" 3 
    (responses e2) List.length;
  make_event_test "Random event sample does not raise exception" true
    ("sample") (random_event_tester); 
  make_event_test "Random event raises exception" false
    ("hello") random_event_tester;
  make_event_test "Random event government does not raise exception" true
    ("government") random_event_tester;
  make_event_test "Inserting into sample description" "sample hello 0"
    (Event.fill_event_description e1 "hello" 0) (Event.description);
  make_event_test 
    "Inserting into sample description 2" "sample Functional Programming 0"
    (Event.fill_event_description e1 fun_prog 0) (Event.description);
  make_event_test "make_name gives a nonempty string" true 
    (Event.make_name ()) check_nonempty;
  make_event_test "investor event 1 has correct investment value" 100000
    (inv_from_var inv_of_event) investment;
  make_event_test "investor event 1 has correct name" "Bane Crobber"
    (inv_from_var inv_of_event) name;
  "Retrieving employee from an investor raises an exception" >::
  (fun _ -> assert_raises (WrongPersonType("investor")) 
      (fun () -> emp_from_var (snd inv_event)));
  "Retrieving investor from an employee raises an exception" >:: 
  (fun _ -> assert_raises (WrongPersonType("employee")) 
      (fun () -> inv_from_var (snd emp_event)));
  make_event_test "Retrieved employee has correct morale amount" 10 
    (emp_from_var emp_of_event) Founding.emp_morale;
  make_event_test "Retrieved employee has correct reputation" 10
    (emp_from_var emp_of_event) Founding.emp_reputation;
]

let comp1 = new_company "Creative Name"
let john = new_employee "John"
let sample_emp_list = 
  [(custom_employee "Joe" 3 10); 
   (custom_employee "Paul" 5 6);
   (custom_employee "Kerber" 9 8)]
let neg_sample_emp_list = 
  [(custom_employee "Bill" (-5) 7); 
   (custom_employee "Brumsted" 2 (-4)); 
   (custom_employee "Marty" (-3) (-6))]

(* Can't really test some of the values like funding, which are randomly 
   generated each time *)


let founding_tests = [
  "Test reputation default value is 50" >:: 
  (fun _ -> assert_equal 50 (Founding.reputation comp1));
  "Test employees default val" >:: 
  (fun _ -> assert_equal [] (employees comp1));
  "Test morale default val" >:: 
  (fun _ -> assert_equal 50 (Founding.morale comp1));
  "Test default investors is the empty list" >:: 
  (fun _ -> assert_equal [] (Founding.investors comp1));
  "Testing adding an employee increases employee list size" >:: 
  (fun _ -> assert_equal 1 (comp1 |> hire_employee "Paul" 1 
                            |> employees |> List.length));
  (* The test below should fail - the default value is 50, so adding an employee
     will change the morale. However idk how to do assert_not_equal in OCAML -ew424
     "Testing adding an employee increases employee list size" >:: 
     (fun _ -> assert_equal 50 (comp1 |> hire_employee "Paul" |> morale)); *)
  "Testing increasing funding produces correct funding value of new company" >:: 
  (fun _ -> assert_equal 5010 (funding (update_category comp1 "funding" 10)));

  "Testing increasing funding works with a negative value" >:: 
  (fun _ -> assert_equal 4990 
      (funding (update_category comp1 "funding" (-10))));

  "Testing increasing reputation produces correct reputation" >:: 
  (fun _ -> assert_equal 55 
      (Founding.reputation (update_category comp1 "reputation" 5)));

  "Testing increasing morale produces correct morale value" >:: 
  (fun _ -> assert_equal 60 
      (Founding.morale (update_category comp1 "morale" 10)));

  "Testing if reputation can be negative" >:: 
  (fun _ -> assert_equal (-50) 
      (Founding.reputation (update_category comp1 "reputation" (-100))));

  "Testing if morale can be negative" >:: 
  (fun _ -> assert_equal (-1) 
      (Founding.morale (update_category comp1 "morale" (-51))));

  "Testing update_category handles employee hiring" >:: 
  (fun _ -> assert_equal 1 
      (List.length (employees (update_category comp1 "employee" 1))));

  "Testing update_category handles multiple employee hiring " >:: 
  (fun _ -> assert_equal 7
      (List.length (employees (update_category comp1 "employee" 7))));

  "Testing update_category handles negative employee hiring and returns an" ^ 
  "empty list" >:: 
  (fun _ -> assert_equal [] 
      (employees (update_category comp1 "employee" (-5))));

  "Testing employee_list generates a list of 3 employees with name John" >:: 
  (fun _ -> assert_equal 4 (List.length (employee_list "John" 3 [john]))); 

  "Testing morale_employees properly sums the sample list." >:: 
  (fun _ -> assert_equal 17 (morale_employees sample_emp_list));

  "Testing reputation_employees properly sums the reputation of the sample" ^
  "employee list." >:: 
  (fun _ -> assert_equal 24 (rep_employees sample_emp_list));

  "Testing morale_employees properly sums the sample employee list and works" ^
  "with negative integers" >:: 
  (fun _ -> assert_equal (-6) (morale_employees neg_sample_emp_list));

  "Testing reputation_employees sums the reputation of the sample employee" ^
  "list-with negative values" >:: 
  (fun _ -> assert_equal (-3) (rep_employees neg_sample_emp_list));

  "Testing check_lost_phase1 returns false on the standard, fresh made company" 
  >:: (fun _ -> assert_equal false (check_lost_phase1 comp1));

]

(*comp1 has stats: funding 5000, reputation 50, morale 50, employees/investores = []*)
let founded1 = found comp1

let resp1 = new_f_response "test1" [("market_cap", Some (1000))]
let resp2 = new_f_response "test2" 
    [("morale", Some(4)); ("reputation", Some(6))]
let resp3 = new_f_response "test3" [("market_cap", Some (-6000))] 
let resp4 = new_f_response "test4" [("management", Some (13))]
let resp5 = new_f_response "test5" [("marketing", Some (-4))]

let event_ads = event_of "demo" 1 "data/events_founded.json"
let event_management = event_of "demo" 2 "data/events_founded.json"




let growth_tests = [ 

  "Testing found successfully transfer over correct funding value to market cap" 
  >:: (fun _ -> assert_equal 5000 (market_cap founded1)); 

  "Testing found successfully transfer over correct reputation value" >::
  (fun _ -> assert_equal 50 (reputation founded1)); 

  "Testing found successfully transfer over correct morale value" >::
  (fun _ -> assert_equal 50 (morale founded1)); 

  "Testing update_founding successfully increases market_cap" >::
  (fun _ -> assert_equal 6000 (market_cap (update_founded founded1 resp1))); 

  "Testing update_founding successfully increases reputation -" ^ 
  "with multiple effect list" 
  >:: (fun _ -> assert_equal 56 (reputation (update_founded founded1 resp2))); 

  "Assuring pulling events from events_founded works and event_ads is correct" 
  >:: (fun _ -> assert_equal 3 (List.length(responses event_ads)));

  "Testing update_founding successfully increases morale - with a multiple" ^
  "effect list and morale being the first element." >::
  (fun _ -> assert_equal 54 (morale (update_founded founded1 resp2))); 

  "Testing update_founding successfully increases management, with a real event"
  ^ " response that updates multiple fields" >::
  (fun _ -> assert_equal 113 (management (update_founded founded1 resp4)));

  "Testing update_founding successfully increases marketing, with a real event"
  >:: (fun _ -> assert_equal 46 (marketing (update_founded founded1 resp5)));

  "Testing check_won returns true on starting company" >:: 
  (fun _ -> assert_equal true (check_won_lost founded1));


]


let suite =
  "test suite for project"  >::: List.flatten [
    event_tests;
    founding_tests;
    growth_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
