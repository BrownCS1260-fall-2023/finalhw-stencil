open Core
open Difftest

let () =
  let difftest_results = run_difftests () in
  let inconsistent_tests =
    List.filter ~f:(Fn.non is_consistent) difftest_results
  in
  if List.is_empty difftest_results then
    Printf.printf
      "You have no tests - write some tests to check the correctness of your \
       implementation.\n"
  else if List.is_empty inconsistent_tests then
    Printf.printf "All %L tests passed!\n" (List.length difftest_results)
  else (
    Printf.printf
      "\n%L out of %L test failed. See inconsistent test(s) below: \n"
      (List.length inconsistent_tests)
      (List.length difftest_results) ;
    PrintBox_text.output stdout (difftest_box_output inconsistent_tests) ;
    exit 1 )
