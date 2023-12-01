open Ast
open Asm.Directive
open Util

let propagate_constants (p : program) = p

let uniquify_variables (p : program) = p

let inline (p : program) = p

let eliminate_common_subexpressions (p : program) = p

let peephole (instrs : directive list) = instrs

type opt_pass =
  | AstPass of (program -> program)
  | AsmPass of (directive list -> directive list)

let all_passes =
  [ ("propagate-constants", AstPass propagate_constants)
  ; ("uniquify-variables", AstPass uniquify_variables)
  ; ("inline", AstPass inline)
  ; ("eliminate-common-subexpressions", AstPass eliminate_common_subexpressions)
  ; ("peephole", AsmPass peephole) ]

exception InvalidPasses of string

let validate_passes (l : string list) =
  (* Must use uniquify-variables before inline or ECS *)
  let rec valid_uniquify = function
    | [] | "uniquify-variables" :: _ ->
        ()
    | (("inline" | "eliminate-common-subexpressions") as p) :: _ ->
        let m =
          Printf.sprintf "'uniquify-variables' must be applied before '%s'" p
        in
        raise (InvalidPasses m)
    | _ :: l ->
        valid_uniquify l
  (* If peephole is specified, it must be the last element *)
  and valid_peephole = function
    | [] | "peephole" :: [] ->
        ()
    | "peephole" :: _ ->
        raise (InvalidPasses "'peephole' must be the last optimization applied")
    | _ :: l ->
        valid_peephole l
  in
  (* The names of all passes must be found in `all_passes` *)
  List.find_opt (fun p -> not (List.mem_assoc p all_passes)) l
  |> Option.iter (fun p ->
         raise (InvalidPasses (Printf.sprintf "Invalid pass '%s'" p)) ) ;
  valid_uniquify l ;
  valid_peephole l ;
  l

let get_passes (pass_spec : string list option) =
  Option.value pass_spec ~default:(List.map fst all_passes)
  |> validate_passes
  |> List.map (fun s -> List.assoc s all_passes)

let apply_ast_pass (prog : program) = function
  | AstPass pass ->
      pass prog
  | _ ->
      prog

let apply_asm_pass (instr : directive list) = function
  | AsmPass pass ->
      pass instr
  | _ ->
      instr

let optimize_ast (prog : program) (pass_spec : string list option) =
  get_passes pass_spec |> List.fold_left apply_ast_pass prog

let optimize_asm (instr : directive list) (pass_spec : string list option) =
  get_passes pass_spec |> List.fold_left apply_asm_pass instr
