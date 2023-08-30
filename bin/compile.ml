open Core
open Filename_unix
open Command_unix
open Csci1260
open Asm

let command =
  Command.basic ~summary:"Compile the given file to an executable"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
      and directory = anon ("directory" %: Filename_unix.arg_type)
      and run = flag "-r" no_arg ~doc:"run the binary" in
      fun () ->
        try
          let text = In_channel.read_all filename in
          let ast =
            if Filename.check_suffix filename ".mlb" then Mlb_syntax.parse text
            else Lisp_syntax.parse text
          in
          let instrs = Compile.compile ast in
          let filename = Filename.basename filename in
          if run then (
            Assemble.eval ~directory ~runtime_text:Runtime.runtime
              ~name:filename ~args:[] ~instrs
            |> function
            | Ok output ->
                printf "%s\n" output
            | Error (Expected error | Unexpected error) ->
                eprintf "%s\n" error ; exit 1 )
          else
            Assemble.build ~directory ~runtime_text:Runtime.runtime
              ~name:filename ~instrs
            |> ignore
        with Error.Stuck _ as e ->
          Printf.eprintf "Error: %s\n" (Exn.to_string e) ;
          exit 1 )

let () = Command_unix.run ~version:"1.0" command
