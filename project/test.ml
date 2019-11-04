open Event
open Founding
open Growth
open Main
open OUnit2

let event_tests = [

]

let suite =
  "test suite for A2"  >::: List.flatten [
    event_tests;
    (* add others*)
  ]

let _ = run_test_tt_main suite
