type token =
  | IDENTIFIER of string
  | KEYWORD of string
  | BOOLEAN of bool
  | ARITH_OP of string
  | INT_CONST of int
  | COMPARISON_OP of string
  | STRING_CONST of string
  | PAREN_OPEN
  | PAREN_CLOSE
  | COMMA
  | EOF

let is_whitespace c = 
  match c with 
  ' ' -> true
  |_ -> false;;

let is_lower a =
  match a with
    'a'..'z' -> true
  | _ -> false ;;

let is_upper a =
  match a with
    'A'..'Z' -> true
    | _ -> false ;;

let is_alpha c = 
  match c with
  'a'..'b' -> true
  | 'A'..'B' -> true
  | _ -> false;;

let is_digit a =
  match a with
    '0'..'9' -> true
    | _ -> false ;;

let keywords = ["if"; "then"; "else"; "let"; "true"; "false"]

(* Function to check if a string is a keyword *)
let is_keyword s = List.mem s keywords

(* Function to tokenize a string *)
let rec tokenize input =
let rec consume_whitespace input =
  match input with
  | c :: cs when is_whitespace c -> consume_whitespace cs
  | _ -> input
  in
  match consume_whitespace input with
  | [] -> [EOF]
  | c :: cs ->
      if is_lower c || c = '_' then
        let rec extract_identifier acc rest =
          match rest with
          | '_' :: '\'' :: cs' ->
              extract_identifier (acc ^ "_'") cs'
          | '\'' :: cs' ->
              extract_identifier (acc ^ "'") cs'
          | c' :: cs' when is_alpha c' || is_digit c' || c' = '_' ->
              extract_identifier (acc ^ String.make 1 c') cs'
          | _ -> (if is_keyword acc then KEYWORD acc else IDENTIFIER acc) :: tokenize rest
        in
        extract_identifier (String.make 1 c) cs
      else if is_digit c then
        let rec extract_number acc rest =
          match rest with
          | c' :: cs' when is_digit c' ->
              extract_number (acc ^ String.make 1 c') cs'
          | _ -> (INT_CONST (int_of_string acc)) :: tokenize rest
        in
        extract_number (String.make 1 c) cs
      else
        match c with
        | '(' -> PAREN_OPEN :: tokenize cs
        | ')' -> PAREN_CLOSE :: tokenize cs
        | ',' -> COMMA :: tokenize cs
        | '"' ->
            let rec extract_string acc rest =
              match rest with
              | '"' :: cs' -> (STRING_CONST acc) :: tokenize cs'
              | c' :: cs' -> extract_string (acc ^ String.make 1 c') cs'
              | _ -> failwith "Unterminated string literal"
            in
            extract_string "" cs
        | _ ->
            let rec extract_operator acc rest =
              match rest with
              | '=' :: cs' ->
                  (match acc with
                  | "<" | ">" | "!" -> (COMPARISON_OP (acc ^ "=")) :: tokenize cs'
                  | _ -> (ARITH_OP acc) :: tokenize ('=' :: cs'))
              | c' :: cs' when not (is_whitespace c') ->
                  extract_operator (acc ^ String.make 1 c') cs'
              | _ -> (ARITH_OP acc) :: tokenize rest
            in
            extract_operator (String.make 1 c) cs

(* Test cases *)
let test_cases = [
"x + 2 * y"; (* identifiers and arithmetic operators *)
"if x > 0 then \"positive\" else \"negative\""; (* if-then-else *)
"true && false"; (* boolean operators *)
"(1, 2, 3)"; (* tuples *)
"x == y"; (* comparison operators *)
"\"hello, world!\""; (* string constants *)
"123 + 456"; (* integer constants *)
"(x', y_1)"; (* identifiers with primes and underscores *)
]

(* Tokenize and print each test case *)
let _ =
List.iter (fun test_case ->
  let tokens = tokenize (String.to_seq test_case |> List.of_seq) in
  List.iter (fun token -> print_endline (match token with
    | IDENTIFIER s -> "IDENTIFIER: " ^ s
    | KEYWORD s -> "KEYWORD: " ^ s
    | BOOLEAN b -> "BOOLEAN: " ^ string_of_bool b
    | ARITH_OP s -> "ARITH_OP: " ^ s
    | INT_CONST n -> "INT_CONST: " ^ string_of_int n
    | COMPARISON_OP s -> "COMPARISON_OP: " ^ s
    | STRING_CONST s -> "STRING_CONST: " ^ s
    | PAREN_OPEN -> "PAREN_OPEN"
    | PAREN_CLOSE -> "PAREN_CLOSE"
    | COMMA -> "COMMA"
    | EOF -> "EOF"
  )) tokens;
  print_endline "---"
) test_cases