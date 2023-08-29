open Tokens
open Ast

exception ParseError of token list

(* [consume_token t toks] ensures that the head of [toks] is [t]. If it is, it
   returns the tail of [toks]. Otherwise, it raises a [ParseError]. *)
let consume_token t toks =
  match toks with
  | t' :: toks' when t = t' ->
      toks'
  | _ ->
      raise (ParseError toks)

(* [call_or_prim f args toks] returns an expression. If [f] is the name of a
   primitive, it will either return [Prim0], [Prim1], or [Prim2] with the
   arguments; if the wrong number of arguments are passed in, it will throw
   an error. If [f] isn't the name of a primitive, it will return [Call] *)
let call_or_prim f args toks =
  match f with
  | "read_num" -> (
    match args with [] -> Prim0 ReadNum | _ -> raise (ParseError toks) )
  | "newline" -> (
    match args with [] -> Prim0 Newline | _ -> raise (ParseError toks) )
  | "add1" -> (
    match args with [arg] -> Prim1 (Add1, arg) | _ -> raise (ParseError toks) )
  | "sub1" -> (
    match args with [arg] -> Prim1 (Sub1, arg) | _ -> raise (ParseError toks) )
  | "is_zero" -> (
    match args with
    | [arg] ->
        Prim1 (IsZero, arg)
    | _ ->
        raise (ParseError toks) )
  | "is_num" -> (
    match args with [arg] -> Prim1 (IsNum, arg) | _ -> raise (ParseError toks) )
  | "is_pair" -> (
    match args with
    | [arg] ->
        Prim1 (IsPair, arg)
    | _ ->
        raise (ParseError toks) )
  | "is_empty" -> (
    match args with
    | [arg] ->
        Prim1 (IsEmpty, arg)
    | _ ->
        raise (ParseError toks) )
  | "left" -> (
    match args with [arg] -> Prim1 (Left, arg) | _ -> raise (ParseError toks) )
  | "right" -> (
    match args with [arg] -> Prim1 (Right, arg) | _ -> raise (ParseError toks) )
  | "print" -> (
    match args with [arg] -> Prim1 (Print, arg) | _ -> raise (ParseError toks) )
  | "pair" -> (
    match args with
    | [arg1; arg2] ->
        Prim2 (Pair, arg1, arg2)
    | _ ->
        raise (ParseError toks) )
  | _ ->
      Call (f, args)

let rec parse_program toks =
  let defns, toks = parse_defns toks in
  let body, toks = parse_expr toks in
  if List.length toks <> 0 then raise (ParseError toks) else {defns; body}

and parse_defns toks = ([], toks)

and parse_expr toks = (True, toks)

let parse s = s |> tokenize |> parse_program
