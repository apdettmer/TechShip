open Event
open Founding
open Growth
open OUnit2
open Yojson.Basic.Util

let e1 = event_of "sample" 0 

let e2 = event_of "investor" 0 
(* changed from 10 -- for future reference, I think we need to increment each 
   time we add an event to make clear bounds for random selection of events *)

let fun_prog = List.nth( Yojson.Basic.from_file "data/wordbank.json"
                         |> member "words" |> to_list) 1 |> to_string

let make_event_test
    (name : string)
    (expected_output : 'b)
    (input : 'a)
    (f : 'a -> 'b) : test =
  name >:: (fun _ ->
      assert_equal expected_output (f input))

let event_tests = [
  make_event_test "category of sample" "sample" e1 Event.category;
  make_event_test 
    "description of sample" "sample string_val int_val" e1 Event.description;
  make_event_test "affected stats list size of sample" 
    2 (Event.affected_stats e1) List.length;
  make_event_test "affected stats list size of investor event e2" 2 
    (Event.affected_stats e2) List.length;
  make_event_test "Response list is 3 elements long" 3 
    (responses e2) List.length;
  make_event_test "Random event sample does not raise exception" ()
    (Event.random_event "sample") ignore; (* exceptions are not caught by ignore*)
  make_event_test "Random event investor does not raise exception" ()
    (Event.random_event "investor") ignore;
  make_event_test "Random event government does not raise exception" ()
    (Event.random_event "government") ignore;
  make_event_test "Inserting into sample description" "sample hello 0"
    (Event.fill_event_description e1 "hello" 0) (Event.description);
  make_event_test 
    "Inserting into sample description 2" "sample Functional Programming 0"
    (Event.fill_event_description e1 fun_prog 0) (Event.description)
]

let comp1 = new_company "Creative Name"
let john = new_employee "John"
let sample_emp_list = [(custom_employee "Joe" 3 10); (custom_employee "Paul" 5 6);
                       (custom_employee "Kerber" 9 8)]
let neg_sample_emp_list = [(custom_employee "Bill" (-5) 7); (custom_employee "Brumsted" 2 (-4)); 
                           (custom_employee "Marty" (-3) (-6))]

(* Can't really test some of the values like funding, which are randomly 
   generated each time *)


let founding_tests = [
  "Test reputation default value is 50" >:: 
  (fun _ -> assert_equal 50 (reputation comp1));
  "Test employees default val" >:: 
  (fun _ -> assert_equal [] (employees comp1));
  "Test morale default val" >:: 
  (fun _ -> assert_equal 50 (morale comp1));
  "Test default investors is the empty list" >:: 
  (fun _ -> assert_equal [] (investors comp1));
  "Testing adding an employee increases employee list size" >:: 
  (fun _ -> assert_equal 1 (comp1 |> hire_employee "Paul" 1 |> employees |> List.length));
  (* The test below should fail - the default value is 50, so adding an employee
     will change the morale. However idk how to do assert_not_equal in OCAML -ew424
     "Testing adding an employee increases employee list size" >:: 
     (fun _ -> assert_equal 50 (comp1 |> hire_employee "Paul" |> morale)); *)
  "Testing increasing funding produces correct funding value of new company" >:: 
  (fun _ -> assert_equal 5010 (funding (update_category comp1 "funding" 10)));

  "Testing increasing funding works with a negative value" >:: 
  (fun _ -> assert_equal 4990 (funding (update_category comp1 "funding" (-10))));

  "Testing increasing reputation produces correct reputation" >:: 
  (fun _ -> assert_equal 55 (reputation (update_category comp1 "reputation" 5)));

  "Testing increasing morale produces correct morale value" >:: 
  (fun _ -> assert_equal 60 (morale (update_category comp1 "morale" 10)));

  "Testing if reputation can be negative" >:: 
  (fun _ -> assert_equal (-50) (reputation (update_category comp1 "reputation" (-100))));

  "Testing if morale can be negative" >:: 
  (fun _ -> assert_equal (-1) (morale (update_category comp1 "morale" (-51))));

  "Testing update_category handles employee hiring" >:: 
  (fun _ -> assert_equal 1 
      (List.length (employees (update_category comp1 "employee" 1))));

  "Testing update_category handles multiple employee hiring " >:: 
  (fun _ -> assert_equal 7
      (List.length (employees (update_category comp1 "employee" 7))));

  "Testing update_category handles negative employee hiring and returns an empty list" >:: 
  (fun _ -> assert_equal [] (employees (update_category comp1 "employee" (-5))));

  "Testing employee_list generates a list of 3 employees with name John" >:: 
  (fun _ -> assert_equal 4 (List.length (employee_list "John" 3 [john]))); 

  "Testing morale_employees properly sums the sample list." >:: 
  (fun _ -> assert_equal 17 (morale_employees sample_emp_list));

  "Testing reputation_employees properly sums the reputation of the sample employee list." >:: 
  (fun _ -> assert_equal 24 (rep_employees sample_emp_list));

  "Testing morale_employees properly sums the sample employee list and works with negative integers" >:: 
  (fun _ -> assert_equal (-6) (morale_employees neg_sample_emp_list));

  "Testing reputation_employees sums the reputation of the sample employee list-with negative values" >:: 
  (fun _ -> assert_equal (-3) (rep_employees neg_sample_emp_list));




]

let suite =
  "test suite for project"  >::: List.flatten [
    event_tests;
    founding_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
