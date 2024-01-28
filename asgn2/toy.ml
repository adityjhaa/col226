type myBool = T | F;;

let myBool2bool b = match b with
    T -> true
  | F -> false
;;

let bool2myBool b = match b with
    true -> T
  | false -> F
;;

let myNot b = match b with
    T -> F
  | F -> T
;;

let myAnd b1 b2 = match b1 with
    T -> b2
  | F -> F
;;

let myOr b1 b2 = match b1 with
    T -> T
  | F -> b2
;;



type exp = 
    Num of int 
  | Bl of myBool
  | Plus of exp * exp 
  | Times of exp * exp
  | And of exp * exp 
  | Or of exp * exp 
  | Not of exp
  | Eq of exp * exp 
  | Gt of exp * exp
;;

let rec ht e = match e with
    Num n -> 0
  | Bl b -> 0
  | Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Times (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | And (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Or (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Not e1 -> 1 + (ht e1)
  | Eq (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Gt(e1, e2) -> 1 + (max (ht e1) (ht e2))
;;

let rec size e = match e with
    Num n -> 1
  | Bl b -> 1
  | Plus (e1, e2) -> 1 + (size e1) + (size e2)
  | Times (e1, e2) -> 1 + (size e1) + (size e2)
  | And (e1, e2) -> 1 + (size e1) + (size e2)
  | Or (e1, e2) -> 1 + (size e1) + (size e2)
  | Not e1 -> 1 + (size e1)
  | Eq (e1, e2) -> 1 + (size e1) + (size e2)
  | Gt(e1, e2) -> 1 + (size e1) + (size e2)
;;

type values = 
    N of int 
  | B of bool 
;;

let rec eval e = match e with
    Num n -> N n
  | Bl b -> B (myBool2bool b)
  | Plus (e1, e2) -> let N n1 = (eval e1)
                    and N n2 = (eval e2)
                  in N (n1 + n2)
  | Times (e1, e2) -> let N n1 = (eval e1)
                    and N n2 = (eval e2)
                  in N (n1*n2)
  | And (e1, e2) -> let B b1 = (eval e1) 
                    and B b2 = (eval e2)
                  in B (b1 && b2)
  | Or (e1, e2) -> let B b1 = (eval e1)
                    and B b2 = (eval e2)
                  in B (b1 || b2) 
  | Not e1 -> let B b1 = (eval e1) 
                  in B (not b1)
  | Eq (e1, e2) -> let N n1 = (eval e1)
                    and N n2 = (eval e2)
                  in B (n1 = n2)
  | Gt (e1, e2) -> let N n1 = (eval e1)
                    and N n2 = (eval e2)
                  in B (n1>n2)  

;;

