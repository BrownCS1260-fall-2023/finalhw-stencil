open Ast

let propagate_constants (p : program) = p

let uniquify_variables (p : program) = p

let inline (p : program) = p

let eliminate_common_subexpressions (p : program) = p

let all_passes =
  [ ("propagate-constants", propagate_constants)
  ; ("uniquify-variables", uniquify_variables)
  ; ("inline", inline)
  ; ("eliminate-common-subexpressions", eliminate_common_subexpressions) ]

exception InvalidPasses

let validate_passes (l : string list) =
  let rec go = function
    | [] ->
        ()
    | "uniquify-variables" :: _ ->
        ()
    | "inline" :: _ ->
        raise InvalidPasses
    | "eliminate-common-subexpressions" :: _ ->
        raise InvalidPasses
    | _ :: l ->
        go l
  in
  go l

let get_passes (pass_spec : string list option) =
  match pass_spec with
  | None ->
      List.map snd all_passes
  | Some l ->
      validate_passes l ;
      List.map (fun s -> List.assoc s all_passes) l

let optimize (prog : program) (pass_spec : string list option) =
  let passes = get_passes pass_spec in
  List.fold_left (fun p f -> f p) prog passes
