open Printf
open Token

let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token lexbuf with
    | EOF -> acc
    | t -> work (t::acc)
  in List.rev (work [])

let pp_token = function
  | PLUS -> "+: arithmetic operator (PLUS)"
  | TIMES -> "*: arithmetic operator (TIMES)"
  | MINUS -> "-: arithmetic operator (MINUS)"
  | BY -> "/: arithmetic operator (BY)"
  | MOD -> "%: arithmetic operator (MOD)"
  | ASSIGN -> "=: assignment operator (ASSIGN)"
  | EQUAL -> "==: comparison operator (EQUAL)"
  | GREATER -> ">: comparison operator (GREATER)"
  | LESS -> "<: comparison operator (LESS)"
  | GTE -> ">=: comparison operator (GTE)"
  | LTE -> "<=: comparison operator (LTE)"
  | UNEQUAL -> "<>: comparison operator (UNEQUAL)"
  | IDENTIFIER i -> sprintf "%s : IDENTIFIER" i
  | KEYWORD v -> sprintf "%s : KEYWORD" v
  | BOOLEAN b -> sprintf "%b : BOOLEAN" b
  | INT n -> sprintf "%d : constant" n
  | OPEN_BRAC -> "OPEN_BRAC"
  | CLOSE_BRAC -> "CLOSE_BRAC"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | EOF -> "EOF"
  | CAPERROR c -> sprintf "%s : ERROR" c
  | ERROR -> "ERROR!"

let main =
  let input = read_line() in
  let lexbuf = Lexing.from_string input in
  let token_list = get_token_list lexbuf in
  List.map pp_token token_list |> List.iter (printf "%s\n")
