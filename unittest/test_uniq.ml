open OUnit2
open Lisp_syntax
open Csci1260.Optimize
open Csci1260.Interp
open Ast

let unique (p : program) =
  let rec all_variables_expr = function
    | Prim1 (_, e) ->
        all_variables_expr e
    | Prim2 (_, e1, e2) ->
        all_variables_expr e1 @ all_variables_expr e2
    | If (test, th, el) ->
        all_variables_expr test @ all_variables_expr th @ all_variables_expr el
    | Let (v, e, body) ->
        [v] @ all_variables_expr e @ all_variables_expr body
    | Do es ->
        es |> List.concat_map all_variables_expr
    | Call (_, es) ->
        es |> List.concat_map all_variables_expr
    | _ ->
        []
  in
  let all_variables_defn defn = defn.args @ all_variables_expr defn.body in
  let all_variables =
    List.concat_map all_variables_defn p.defns @ all_variables_expr p.body
  in
  List.length all_variables = List.length (List.sort_uniq compare all_variables)

let uniquifies (p : string) =
  p
  >:: fun _ ->
  let prog = parse p in
  let uniquified = uniquify_variables prog in
  assert_bool ("Not all unique " ^ p) (unique uniquified) ;
  let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" in
  assert_equal (interp_io prog input) (interp_io uniquified input)

let tests =
  "uniquify"
  >::: [ "basics"
         >::: [ uniquifies "(let ((x 2)) x)"
              ; uniquifies
                  "(let ((x (read-num))) (+ x (let ((x (read-num))) x)))" ]
       ; "multiple functions"
         >::: [ uniquifies
                  "(define (f x) x) (define (g x) x) (+ (g (read-num)) (f \
                   (read-num)))" ] ]

let _ = run_test_tt_main tests
