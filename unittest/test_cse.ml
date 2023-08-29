open OUnit2
open Lisp_syntax
open Csci1260.Optimize
open Csci1260.Interp
open Ast

let should_not_change (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  assert_bool
    ("Shouldn't optimize " ^ p)
    (prog = eliminate_common_subexpressions prog)

let should_change (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  let optimized = eliminate_common_subexpressions prog in
  assert_bool ("Should optimize " ^ p) (prog <> optimized) ;
  let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" in
  assert_equal (interp_io prog input) (interp_io optimized input)

let tests =
  "common subexpression elimination"
  >::: [ "basics"
         >::: [ should_not_change "(+ 1 2)"
              ; should_change "(+ (- (+ 4 3) 2) (- (+ 4 3) 2))"
              ; should_not_change "(sub1 4)" ]
       ; "side effects"
         >::: [ should_not_change
                  "(+ (- 4 (+ 3 (read-num))) (- 4 (+ 3 (read-num))))"
              ; should_change
                  "(let ((x (read-num))) (+ (- 4 (+ 3 x)) (- 4 (+ 3 x))))" ]
       ; "whole program"
         >::: [ should_change
                  "(define (f x) (+ (+ 3 (- 4 x)) (+ 3 (- 4 x)))) (f \
                   (read-num))"
              ; should_not_change
                  "(define (f x) (+ 3 (- 4 1))) (define (g x) (+ 3 (- 4 1))) \
                   (+ (f 2) (g 2))" ] ]

let _ = run_test_tt_main tests
