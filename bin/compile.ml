open Filename_unix
open Command_unix
open Csci1260
open Asm

let command =
  Command.basic ~summary:"Compile the given file to an executable"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
      and directory = anon ("directory" %: Filename_unix.arg_type)
      and explicit_passes =
        flag "-p" (listed string)
          ~doc:
            "optimization passes to use, overrides default which is to apply \
             all"
      and no_opt = flag "-noopt" no_arg ~doc:"disables all optimizations"
      and run = flag "-r" no_arg ~doc:"run the binary" in
      fun () ->
        if explicit_passes <> [] && no_opt then (
          Printf.eprintf "Error: Cannot pass -p with -noopt\n" ;
          exit 1 ) ;
        try
          let text = Core.In_channel.read_all filename in
          let ast =
            if Filename.check_suffix filename ".mlb" then Mlb_syntax.parse text
            else Lisp_syntax.parse text
          in
          let passes =
            match explicit_passes with
            | [] ->
                if no_opt then Some [] else None
            | _ ->
                Some explicit_passes
          in
          let ast = Optimize.optimize_ast ast passes in
          let instrs = Compile.compile ast in
          let instrs = Optimize.optimize_asm instrs passes in
          let filename = Filename.basename filename in
          if run then (
            Assemble.eval ~directory ~runtime_text:Runtime.runtime
              ~name:filename ~args:[] ~instrs
            |> function
            | Ok output ->
                Printf.printf "%s\n" output
            | Error (Expected error | Unexpected error) ->
                Printf.eprintf "%s\n" error ;
                exit 1 )
          else
            Assemble.build ~directory ~runtime_text:Runtime.runtime
              ~name:filename ~instrs
            |> ignore
        with (Error.Stuck _ | Optimize.InvalidPasses _) as e ->
          Printf.eprintf "Error: %s\n" (Printexc.to_string e) ;
          exit 1 )

let () = Command_unix.run ~version:"1.0" command
