{
    open Token
}


rule token = parse
    [' ' '\t' '\n']
        { token lexbuf }
    | ['+']
        { PLUS }
    | ['-']
        { MINUS }
    | ['*']
        { TIMES }
    | ['/']
        { BY }
    | ['%']
        { MOD }
    | ['=']
        { ASSIGN }
    | "=="
        { EQUAL }
    | ['>']
        { GREATER }
    | ['<']
        { LESS }
    | ">="
        { GTE }
    | "<="
        { LTE }
    | "<>"
        { UNEQUAL }
    | "true"
        { BOOLEAN true }
    | "false"
        { BOOLEAN false }
    | ['0'-'9']+ as n
        { INT (int_of_string n) }
    | ['(']
        { OPEN_BRAC }
    | [')']
        { CLOSE_BRAC }
    | [',']
        { COMMA }
    | [';']
        { SEMICOLON }
    | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as v
        { if is_key v then KEYWORD v else IDENTIFIER v }
    | eof 
        { EOF }
    | ['A'-'Z' '\''] ['A'-'Z' '\'']* as c
        { CAPERROR c }
    | _ 
        { ERROR }

