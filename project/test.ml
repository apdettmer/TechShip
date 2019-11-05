open Event
open Founding
open Growth
open Main
open OUnit2


let e1 = event_of "sample" 0 
let e2 = event_of "investor" 10

let make_event_test
    (name : string)
    (expected_output : 'b)
    (input : 'a)
    (f : 'a -> 'b) : test =
  name >:: (fun _ ->
      assert_equal expected_output (f input))

let event_tests = [
  make_event_test "category of sample" "sample" e1 Event.category;
  make_event_test "description of sample" "sample" e1 Event.description;
  make_event_test "affected stats list size of sample" 
    2 (Event.affected_stats e1) List.length;
  make_event_test "affected stats list size of investor event e2" 2 
    (Event.affected_stats e2) List.length;
  make_event_test "Response list is 3 elements long" 3 
    (responses e2) List.length;
]

let comp1 = new_company "Creative Name"

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
  (fun _ -> assert_equal 1 (comp1 |> hire_employee "Paul" |> employees |> List.length));
  (* The test below should fail - the default value is 50, so adding an employee
     will change the morale. However idk how to do assert_not_equal in OCAML -ew424
     "Testing adding an employee increases employee list size" >:: 
     (fun _ -> assert_equal 50 (comp1 |> hire_employee "Paul" |> morale)); *)

]

let suite =
  "test suite for A2"  >::: List.flatten [
    event_tests;
    founding_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
