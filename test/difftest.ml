open Core

type program_output = (string, unit) Result.t

let string_of_program_output = function
  | Ok output ->
      output
  | Error () ->
      "Error"

let wipe_tmp () =
  let tmpdir = "tmp" in
  match Sys_unix.file_exists tmpdir with
  | `Yes ->
      Sys_unix.readdir tmpdir
      |> Array.iter ~f:(fun x -> Sys_unix.remove (tmpdir ^ "/" ^ x))
  | _ ->
      Core_unix.mkdir ~perm:0o777 tmpdir

type diff_result =
  { program_name: string
  ; expected_output: program_output option
  ; interp_output: program_output
  ; compiler_output: program_output }

let run_exit_status_and_output ?stdin ?(timeout = "10s") cmd ~args :
    program_output =
  wipe_tmp () ;
  let open Shexp_process in
  (* Pipes (exit_code, output) to the eval *)
  match
    eval
      (pipe_both
         (err_to_out
            ( match stdin with
            | Some input ->
                pipe (run "echo" [input])
                  (run_exit_code "timeout" ([timeout; cmd] @ args))
            | None ->
                run_exit_code "timeout" ([timeout; cmd] @ args) ) )
         read_all )
  with
  | 0, output ->
      if String.is_substring output ~substring:"Error" then Error ()
      else Ok (String.strip output)
  | 124, _ ->
      Ok "Test timed out"
  | _, _ ->
      Error ()

let interp ?stdin ~interpreter_path filepath =
  run_exit_status_and_output ?stdin interpreter_path ~args:[filepath]

let compile_and_run ?stdin ~compiler_path filepath =
  run_exit_status_and_output ?stdin compiler_path
    ~args:["-r"; filepath; "examples_compiled"]

let test_program_names ~example_dir =
  Sys_unix.ls_dir example_dir
  |> List.filter ~f:(fun x ->
         match Filename.split_extension x with
         | _, Some ext ->
             List.mem ["lisp"; "mlb"] ext ~equal:String.equal
         | _ ->
             false )
  |> List.sort ~compare:String.compare

let get_expected_output ~example_dir program_name : program_output option =
  let out_file = program_name ^ ".out" in
  let err_file = program_name ^ ".err" in
  let out_file_exists = Sys_unix.file_exists (example_dir ^ "/" ^ out_file) in
  let err_file_exists = Sys_unix.file_exists (example_dir ^ "/" ^ err_file) in
  match (out_file_exists, err_file_exists) with
  | `Yes, _ ->
      Some
        (Ok (String.strip (In_channel.read_all (example_dir ^ "/" ^ out_file))))
  | _, `Yes ->
      Some (Error ())
  | _ ->
      None

let get_input ~example_dir program_name : string option =
  let input_file = program_name ^ ".in" in
  let input_file_exists =
    Sys_unix.file_exists (example_dir ^ "/" ^ input_file)
  in
  match input_file_exists with
  | `Yes ->
      Some (In_channel.read_all (example_dir ^ "/" ^ input_file))
  | _ ->
      None

let run_single_test ~example_dir ~interpreter_path ~compiler_path program_name =
  let program_name_no_ext = Filename.chop_extension program_name in
  let input = get_input ~example_dir program_name_no_ext in
  let interp_output =
    interp ?stdin:input ~interpreter_path (example_dir ^ "/" ^ program_name)
  in
  let compiler_output =
    compile_and_run ?stdin:input ~compiler_path
      (example_dir ^ "/" ^ program_name)
  in
  let expected_output = get_expected_output ~example_dir program_name_no_ext in
  { program_name= program_name_no_ext
  ; expected_output
  ; interp_output
  ; compiler_output }

let run_difftests ?(example_dir = "./examples")
    ?(interpreter_path = "./bin/interp.exe")
    ?(compiler_path = "./bin/compile.exe") () =
  test_program_names ~example_dir
  |> List.map ~f:(run_single_test ~example_dir ~interpreter_path ~compiler_path)

(** Displays results in a box*)
let difftest_box_output difftest_results =
  let result_rows_array =
    difftest_results
    |> List.map ~f:(fun res ->
           [| res.program_name
            ; ( match res.expected_output with
              | Some r ->
                  string_of_program_output r
              | None ->
                  "None Specified" )
            ; string_of_program_output res.interp_output
            ; string_of_program_output res.compiler_output |] )
  in
  PrintBox.grid
    ~pad:(PrintBox.pad' ~col:1 ~lines:0)
    (Array.of_list
       ( [ [|"Test"; "Expected"; "Interpreter"; "Compiler"|]
           |> Array.map ~f:(fun (x : string) ->
                  PrintBox.text_with_style PrintBox.Style.bold x ) ]
       @ ( result_rows_array
         |> List.map
              ~f:(Array.map ~f:(fun (x : string) -> PrintBox.sprintf "%s" x)) )
       ) )
  |> PrintBox.frame

let is_consistent difftest_result =
  let ( = ) = Result.equal String.equal (fun () () -> true) in
  match difftest_result with
  | { program_name= _
    ; expected_output= Some expected
    ; interp_output= interp
    ; compiler_output= compiler } ->
      expected = interp && interp = compiler
  | { program_name= _
    ; expected_output= None
    ; interp_output= interp
    ; compiler_output= compiler } ->
      interp = compiler
