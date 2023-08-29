open OUnit2
open Lisp_syntax
open Csci1260.Optimize
open Csci1260.Interp
open Ast

let should_not_change (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  assert_bool ("Shouldn't optimize " ^ p) (prog = inline prog)

let should_inline n (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  let optimized = inline prog in
  assert_bool ("Should optimize " ^ p) (prog <> optimized) ;
  assert_equal (List.length prog.defns - List.length optimized.defns) n ;
  let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" in
  assert_equal (interp_io prog input) (interp_io optimized input)

let function_body_of_size n =
  let rec go = function
    | 0 ->
        "x"
    | n ->
        "(+ " ^ go (n - 1) ^ " " ^ go (n - 1) ^ ")"
  in
  "(define (f x) " ^ go n ^ ")"

let program_body_of_size n =
  let rec go = function
    | 0 ->
        "(f 3)"
    | n ->
        "(+ " ^ go (n - 1) ^ " " ^ go (n - 1) ^ ")"
  in
  go n

let program_of_size n = function_body_of_size n ^ " " ^ program_body_of_size n

let tests =
  "inlining"
  >::: [ "basics"
         >::: [ should_inline 1 "(define (f x) x) (f 2)"
              ; should_not_change
                  "(define (f x) (if (< x 2) x (+ x (f (sub1 x))))) (f 2)"
              ; should_inline 1
                  "(define (g x) x) (define (f x) (if (< x 2) x (+ x (f (sub1 \
                   x))))) (f (g 2))" ]
       ; "heuristic"
         >::: [ should_inline 1 (program_of_size 1)
              ; should_not_change (program_of_size 10) ]
       ; "multiple"
         >::: [ should_inline 2
                  "(define (f x) x) (define (g x) x) (+ (f 2) (g 2))"
              ; should_inline 2
                  "(define (f x) x) (define (g x) (f x)) (+ (f 2) (g 2))" ] ]

let _ = run_test_tt_main tests
