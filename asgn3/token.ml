type token = 
| IDENTIFIER of string
| KEYWORD of string
| BOOLEAN of bool
| INT of int
| PLUS
| MINUS
| TIMES
| BY
| MOD
| ASSIGN
| EQUAL
| GREATER
| LESS
| GTE
| LTE
| UNEQUAL
| OPEN_BRAC
| CLOSE_BRAC
| SEMICOLON
| COMMA
| EOF
| CAPERROR of string
| ERROR
;;

let keywords = ["let"; "if"; "then"; "else"; "pair"; "first"; "second"; "for"; "while"; "print"];;

let is_key s = List.mem s keywords;;
