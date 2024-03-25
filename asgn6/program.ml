type variable = V of string;;

type exp = Num of int | Bl of bool | V of string | Plus of exp * exp | Minus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Gt of exp * exp | Lt of exp * exp | Eq of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp | Snd of exp | Abs of string * exp | App of exp * exp;;

type opcode = LDN of int | LDB of bool | LOOKUP of string | PLUS | TIMES | MINUS | AND | OR | NOT | GT | LT | EQ | COND of opcode list * opcode list | PAIR | FST | SND | APP | MKCLOS of string * (opcode list) | RET;;

type values = N of int | B of bool | P of values * values | Clos of variable * code * environment
and stack = values list
and environment = (variable * values) list
and code = opcode list
and dump = (stack * environment * code) list;;

exception Var_not_in_scope of variable;;
exception Stuck of stack * environment * code * dump;;

let rec find (v:variable) (e:environment) : values = 
  match e with
    [] -> raise (Var_not_in_scope v)
  | (x, y)::t -> if v = x then y else find v t
;;

let rec compile (e:exp) : code = 
  match e with
    Num n             -> [LDN n]
  | Bl b              -> [LDB b]
  | V x               -> [LOOKUP x]
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

let rec stkmc (s:stack) (e:environment) (c:code) (d:dump)  : values =
  match (s, c, d) with
    v::_, [ ] , _ -> v
  | s, (LDN n)::c', _                    -> stkmc ((N n)::s) e c' d
  | s, (LDB b)::c', _                    -> stkmc ((B b)::s) e c' d
  | s, (LOOKUP x)::c', _                 -> stkmc ((find (V x) e)::s) e c' d
  | (N n2)::(N n1)::s', PLUS::c', _      -> stkmc (N(n1+n2)::s') e c' d
  | (N n2)::(N n1)::s', MINUS::c', _     -> stkmc (N(n1-n2)::s') e c' d
  | (N n2)::(N n1)::s', TIMES::c', _     -> stkmc (N(n1*n2)::s') e c' d
  | (B b2)::(B b1)::s', AND::c', _       -> stkmc (B(b1 && b2)::s') e c' d
  | (B b2)::(B b1)::s', OR::c', _        -> stkmc (B(b1 || b2)::s') e c' d
  | (B b0)::s', NOT::c', _               -> stkmc (B(not b0)::s') e c' d
  | (N n2)::(N n1)::s', GT::c', _        -> stkmc (B(n1>n2)::s') e c' d
  | (N n2)::(N n1)::s', LT::c', _        -> stkmc (B(n1<n2)::s') e c' d
  | (N n2)::(N n1)::s', EQ::c', _        -> stkmc (B(n1=n2)::s') e c' d
  | (B true)::s', COND(c1, c2)::c', _    -> stkmc s' e (c1 @ c') d
  | (B false)::s', COND(c1, c2)::c', _   -> stkmc s' e (c2 @ c') d
  | v1::v2::s', PAIR::c', _              -> stkmc (P(v1, v2)::s') e c' d
  | (P(v1, _))::s', FST::c', _           -> stkmc (v1::s') e c' d
  | (P(_, v2))::s', SND::c', _           -> stkmc (v2::s') e c' d
  | s, MKCLOS(x, c')::c0, _              -> stkmc (Clos(V x, c', e)::s) e c0 d
  | v::Clos(V x, c', e')::s, APP::c0, d  -> stkmc [] ((V x, v)::e') c' ((s, e, c0)::d)
  | v::s', RET::c', (s, e, c0)::d        -> stkmc (v::s) e c0 d
  | _, _, _                              -> raise (Stuck (s, e, c, d))
;;

let secd (exp:exp) (e:environment) : values = 
  stkmc [] e (compile exp) []
;;
