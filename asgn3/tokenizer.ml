type token = 
  | Identifier of string
  | Keyword of string
  | Int_cons of int
  | Int_Op of string
  | Bool_cons of bool
  | Bool_Op of string
  | Open_brac
  | Close_brac 
  | EOF
;;

let is_whitespace c = 
  match c with 
  " " | "\n"| "\t" -> true
  |_ -> false
;;

let is_lower a =
  match a with
    'a'..'z' -> true
  | _ -> false
;;

let is_upper a =
  match a with
    'A'..'Z' -> true
    | _ -> false
;;

let is_alpha c = 
  match c with
  'a'..'b' -> true
  | 'A'..'B' -> true
  | _ -> false
;;

let is_digit a =
  match a with
    '0'..'9' -> true
    | _ -> false
;;

let keywords = ["let";"if";"then";"else";"pair";"first";"second"];;



let s = read_line();;

let () = print_endline(consume_whitespace s)

