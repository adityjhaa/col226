type variable = V of string;;

type exp = Num of int | Bl of bool | V of string | Plus of exp * exp | Minus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Gt of exp * exp | Lt of exp * exp | Eq of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp * exp | Snd of exp * exp | Abs of string * exp | App of exp * exp;;

type opcode = LDN of int | LDB of bool | LOOKUP of string | PLUS | TIMES | MINUS | AND | OR | NOT | GT | LT | EQ | COND of opcode list * opcode list | PAIR | FST | SND | APP | MKCLOS of string * (opcode list) | RET;;

type values = N of int | B of bool | P of values * values | Clos of variable * exp * environment
and stack = values list
and environment = (variable * values) list
and code = opcode list
and dump = (stack * environment * code) list;;


