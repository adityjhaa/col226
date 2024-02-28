(* Source : https://github.com/david-broman/ocaml-examples/blob/main/compilers/parser_ocamlyacc *)

let main =
  let lexbuf = Lexing.from_channel stdin in
  let res = Parser.main Lexer.token lexbuf in
  Printf.printf "Result: %d\n" res

