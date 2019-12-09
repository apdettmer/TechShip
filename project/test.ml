open Event
open Founding
open Growth
open OUnit2
open Yojson.Basic.Util

let e1 = event_of "sample" 0 

let r1 = match responses e1 with 
  | h::t -> h 
  | _ -> failwith "Sample esponse not found"

let e2 = event_of "investor" 0 

let comp_sample = new_company "sample comp"
let up_comp_sample = update_company r1 comp_sample

let fun_prog = List.nth( Yojson.Basic.from_file "data/wordbank.json"
                         |> member "words" |> to_list) 1 |> to_string

let make_event_test
    (name : string)
    (expected_output : 'b)
    (input : 'a)
    (f : 'a -> 'b) : test =
  name >:: (fun _ ->
      assert_equal expected_output (f input))

let random_event_tester category = 
  try ignore(random_event category); true 
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
    ("sample") random_event_tester; (* exceptions are not caught by ignore*)
  make_event_test "Random event does raises exception" false
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
  make_event_test "updating sample company increases funding" 5010
    (up_comp_sample) (funding);
  make_event_test "updating sample company increases morale" 55
    (up_comp_sample) (Founding.morale);
]

let comp1 = new_company "Creative Name"
let john = new_employee "John"
let sample_emp_list = [(custom_employee "Joe" 3 10); (custom_employee "Paul" 5 6);
                       (custom_employee "Kerber" 9 8)]
let neg_sample_emp_list = [(custom_employee "Bill" (-5) 7); 
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
  (fun _ -> assert_equal 4990 (funding (update_category comp1 "funding" (-10))));

  "Testing increasing reputation produces correct reputation" >:: 
  (fun _ -> assert_equal 55 (Founding.reputation (update_category comp1 "reputation" 5)));

  "Testing increasing morale produces correct morale value" >:: 
  (fun _ -> assert_equal 60 (Founding.morale (update_category comp1 "morale" 10)));

  "Testing if reputation can be negative" >:: 
  (fun _ -> assert_equal (-50) (Founding.reputation (update_category comp1 "reputation" (-100))));

  "Testing if morale can be negative" >:: 
  (fun _ -> assert_equal (-1) (Founding.morale (update_category comp1 "morale" (-51))));

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

(*comp1 has stats: funding 5000, reputation 50, morale 50, employees/investores = []*)
let founded1 = found comp1

let resp1 = new_f_response "test1" [("market_cap", Some (1000))]
let resp2 = new_f_response "test2" [("morale", Some(5)); ("reputation", Some(6))]


let growth_tests = [ 

  "Testing found successfully transfer over correct funding value to market cap" >::
  (fun _ -> assert_equal 5000 (market_cap founded1)); 

  "Testing found successfully transfer over correct reputation value" >::
  (fun _ -> assert_equal 5000 (market_cap founded1)); 

  "Testing found successfully transfer over correct morale value" >::
  (fun _ -> assert_equal 5000 (market_cap founded1)); 

  "Testing update_founding successfully increases market_cap" >::
  (fun _ -> assert_equal 6000 (market_cap (update_founded founded1 resp1))); 

  "Testing update_founding successfully increases reputation - with multiple effect list" >::
  (fun _ -> assert_equal 56 (reputation (update_founded founded1 resp2))); 



]


let suite =
  "test suite for project"  >::: List.flatten [
    event_tests;
    founding_tests;
    growth_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
