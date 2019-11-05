open Event
open Founding
open Growth
open Main
open OUnit2


let e1 = event_of "sample" 0 

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
    2 (Event.affected_stats e1) List.length
]

let comp1 = new_company "Creative Name"

let founding_tests = [
  "Test reputation default value is 50" >:: 
  (fun _ -> assert_equal 50 (reputation comp1));
  "Test employees default val" >:: 
  (fun _ -> assert_equal [] (employees comp1));
  "Test morale default val" >:: 
  (fun _ -> assert_equal 50 (morale comp1));
  "Test default investors is the empty list" >:: 
  (fun _ -> assert_equal [] (investors comp1));
]

let suite =
  "test suite for A2"  >::: List.flatten [
    event_tests;
    founding_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
