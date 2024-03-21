type exp = Num of int | Bl of bool | V of string | Plus of exp * exp | Minus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Gt of exp * exp | Lt of exp * exp | Eq of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp * exp | Snd of exp * exp ;;

type values = N of int | B of bool | P of values * values;;

type opcode = LDN of int | LDB of bool | LOOKUP of string | | PLUS | TIMES | MINUS | AND | OR | NOT | GT | LT | EQ | COND of opcode list * opcode list | PAIR | FST | SND;;

type stack = values list;;

type environment = (var of string * values) list;;

type code = opcode list;;

type dump = (stack * environment * code) list;;


