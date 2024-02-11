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
        { DIV }
    | ['=']
        { ASSIGN }
    | ["=="]
        { EQUAL }
    | [">"]
        { GREATER }
    | ["<"]
        { LESS }
    | [">="]
        { GTE }
    | ["<="]
        { LTE }
    | ['0'-'9']+
        { INT }
    | ["true" "TRUE" "True"]
        { TRUE }
    | ["false" "FALSE" "False"]
        { FALSE }
    | []

    | ['(']
        { OPEN_BRAC }
    | [')']
        { CLOSE_BRAC }
    | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
        { IDENTIFIER }

    