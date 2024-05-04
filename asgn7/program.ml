type variable = V of string;;

type exp = Num of int | Bl of bool | V of string | Plus of exp * exp | Minus of exp * exp | Times of exp * exp | Div of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Gt of exp * exp | Lt of exp * exp | Eq of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp | Snd of exp | Abs of string * exp | App of exp * exp;;

type closure = Clos of exp * table
and table = (variable * closure) list;;

type values = N of int | B of bool | P of values * values | VClos of variable * exp * table;;

type stack = closure list;;

exception Var_not_in_scope of variable;;
exception Stuck of exp * table;;
exception ExpressionError;;

(* look-up *)
let rec lookup (v:variable) (t:table) : closure = 
  match t with
  | [] -> raise (Var_not_in_scope v)
  | (a, c)::r -> if v = a then c
  else lookup v r
;; 

let rec eval (c:closure) (s:stack) : closure = 
  match c, s with
  | Clos(Num _, _), _            -> c
  | Clos(Bl _, _), _             -> c
  | Clos(V v, t), _              -> eval (lookup (V v) t) s
  | Clos(Plus(e1, e2), t), _     -> Clos(add (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t)
  | Clos(Minus(e1, e2), t), _    -> Clos(sub (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t)
  | Clos(Times(e1, e2), t), _    -> Clos(mul (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t)
  | Clos(Div(e1, e2), t), _      -> Clos(div (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t)
  | Clos(And(e1, e2), t), _      -> Clos(and' (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t) 
  | Clos(Or(e1, e2), t), _       -> Clos(or' (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t) 
  | Clos(Not(e0), t), _          -> Clos(not' (eval (Clos(e0, t)) s) , t) 
  | Clos(Gt(e1, e2), t), _       -> Clos(gt (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t) 
  | Clos(Lt(e1, e2), t), _       -> Clos(lt (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t) 
  | Clos(Eq(e1, e2), t), _       -> Clos(eq (eval (Clos(e1, t)) s) (eval (Clos(e2, t)) s), t) 
  | Clos(IfTE(e1, e2, e3), t), _ -> if (ifte (eval (Clos(e1, t)) s)) then (eval (Clos(e2, t)) s) else (eval (Clos(e3, t)) s)
  | Clos(Pair(e1, e2), t), _     -> Clos(Pair(e1, e2), t)
  | Clos(Fst(e0), t), _          -> Clos(fst' (eval (Clos(e0, t)) s), t)
  | Clos(Snd(e0), t), _          -> Clos(snd' (eval (Clos(e0, t)) s), t)
  | Clos(Abs(_, _), _), []       -> c
  | Clos(Abs(x, e), t), cl::s'   -> eval (Clos(e, (V x, cl)::t)) s'
  | Clos(App(e1, e2), t), _      -> eval (Clos(e1, t)) (Clos(e2, t)::s)
and
(* evaluating functions *)
add (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Num(n1 + n2)
  | _ -> raise ExpressionError
and
sub (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Num(n1 - n2)
  | _ -> raise ExpressionError
and
mul (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Num(n1 * n2)
  | _ -> raise ExpressionError
and
div (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Num(n1 / n2)
  | _ -> raise ExpressionError
and
and' (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Bl b1, _), Clos(Bl b2, _) -> Bl(b1 && b2)
  | _ -> raise ExpressionError
and
or' (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Bl b1, _), Clos(Bl b2, _) -> Bl(b1 || b2)
  | _ -> raise ExpressionError
  and
not' (c0:closure) : exp = 
  match c0 with
  | Clos(Bl b0, _) -> Bl(not b0)
  | _ -> raise ExpressionError
and
gt (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Bl(n1 > n2)
  | _ -> raise ExpressionError
and
lt (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Bl(n1 < n2)
  | _ -> raise ExpressionError
and
eq (c1:closure) (c2:closure) : exp = 
  match (c1, c2) with
  | Clos(Num n1, _), Clos(Num n2, _) -> Bl(n1 = n2)
  | _ -> raise ExpressionError
and
ifte (c:closure) : bool = 
  match c with
  | Clos(Bl b, _) -> b
  | _ -> raise ExpressionError
and
fst' (c0:closure) : exp =
  match c0 with
  | Clos(Pair(e1, e2), t) -> let Clos(e, a) = eval (Clos(e1, t)) [] in e
  | _ -> raise ExpressionError
and
snd' (c0:closure) : exp =
  match c0 with
  | Clos(Pair(e1, e2), t) -> let Clos(e, a) = eval (Clos(e2, t)) [] in e
  | _ -> raise ExpressionError
;;

(* The Krivine Machine *)

let rec krivine (e:exp) (t:table) : values =
  match (eval (Clos(e, t)) []) with
  | Clos(Num n, _)        -> N n
  | Clos(Bl b, _)         -> B b
  | Clos(Pair(e1, e2), _) -> let a = krivine e1 [] and b = krivine e2 [] in P(a, b)
  | Clos(Abs(x, ex), t)   -> VClos(V x, ex, t)
  | _ -> raise (Stuck(e, t))
;;

