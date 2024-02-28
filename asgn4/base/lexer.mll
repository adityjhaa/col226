{
  open Parser
}

rule token = parse
     [' ' '\t' '\n']
       { token lexbuf }
   | ['+']
       { ADD }
   | ['*']
       { MUL }
   | ['0'-'9']+ as lxm
       { INT(int_of_string lxm) }
   | ['(']
       { LPAREN }
   | [')']
       { RPAREN }
   | eof | ['x']
       { EOF }

