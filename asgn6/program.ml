type variable = V of string;;

type exp = Num of int | Bl of bool | V of string | Plus of exp * exp | Minus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Gt of exp * exp | Lt of exp * exp | Eq of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp | Snd of exp | Abs of string * exp | App of exp * exp;;

type opcode = LDN of int | LDB of bool | LOOKUP of string | PLUS | TIMES | MINUS | AND | OR | NOT | GT | LT | EQ | COND of opcode list * opcode list | PAIR | FST | SND | APP | MKCLOS of string * (opcode list) | RET;;

type values = N of int | B of bool | P of values * values | Clos of variable * exp * environment
and stack = values list
and environment = (variable * values) list
and code = opcode list
and dump = (stack * environment * code) list;;

exception Var_not_in_scope of variable;;
exception Stuck;;

let rec find v (e:environment) = 
  match e with
    [] -> raise (Var_not_in_scope v)
  | (x, y)::t -> if v = x then y else find v t
;;

let rec compile (e:exp) = 
  match e with
    Num n             -> [LDN(n)]
  | Bl b              -> [LDB(b)]
  | V x               -> [LOOKUP(x)]
  | Plus(e1, e2)      -> (compile e1) @ (compile e2) @ [PLUS]
  | Minus(e1, e2)     -> (compile e1) @ (compile e2) @ [MINUS]
  | Times(e1, e2)     -> (compile e1) @ (compile e2) @ [TIMES]
  | And(e1, e2)       -> (compile e1) @ (compile e2) @ [AND]
  | Or(e1, e2)        -> (compile e1) @ (compile e2) @ [OR]
  | Not e0            -> (compile e0) @ [NOT]
  | Gt(e1, e2)        -> (compile e1) @ (compile e2) @ [GT]
  | Lt(e1, e2)        -> (compile e1) @ (compile e2) @ [LT]
  | Eq(e1, e2)        -> (compile e1) @ (compile e2) @ [EQ]
  | IfTE(e1, e2, e3)  -> (compile e1) @ [COND(compile e2, compile e3)]
  | Pair (e1, e2)     -> (compile e1) @ (compile e2) @ [PAIR]
  | Fst e0            -> (compile e0) @ [FST]
  | Snd e0            -> (compile e0) @ [SND]
  | Abs(x, e0)        -> [MKCLOS(x, (compile e0) @ [RET])]
  | App(e1, e2)       -> (compile e1) @ (compile e2) @ [APP] 
;;

