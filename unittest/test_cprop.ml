open OUnit2
open Lisp_syntax
open Csci1260.Optimize
open Csci1260.Interp
open Ast

let should_not_change (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  assert_bool ("Shouldn't optimize " ^ p) (prog = propagate_constants prog)

let should_change (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  let optimized = propagate_constants prog in
  assert_bool ("Should optimize " ^ p) (prog <> optimized) ;
  let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" in
  assert_equal (interp_io prog input) (interp_io optimized input)

let tests =
  "constant propagation"
  >::: [ "basics"
         >::: [ should_not_change "(read-num)"
              ; should_change "(+ 2 2)"
              ; should_change "(- 3 4)"
              ; should_change "(add1 4)"
              ; should_not_change "(add1 (read-num))"
              ; should_change "(sub1 4)" ]
       ; "print"
         >::: [should_change "(print (+ 2 3))"; should_not_change "(print 3)"]
       ; "if"
         >::: [ should_change "(if true 1 2)"
              ; should_change "(if (< 2 3) 2 3)"
              ; should_not_change "(if (= (read-num) 0) 3 5)"
              ; should_change "(if (= (read-num) 0) 3 (+ 3 5))" ]
       ; "let"
         >::: [ should_change "(let ((x 2)) x)"
              ; should_not_change "(let ((x (read-num))) x)"
              ; should_change "(let ((x (read-num))) (+ 2 2))" ]
       ; "whole program"
         >::: [ should_not_change "(define (f x) (add1 x)) (print (f 1))"
              ; should_change "(define (f x) (+ 2 3)) (print (f 1))" ] ]

let _ = run_test_tt_main tests
