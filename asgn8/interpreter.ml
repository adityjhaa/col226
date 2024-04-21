(* Structure of the program *)

type variable = string;;
type symbol = string;;
type signature = (symbol * int) list;;
type term = Var of variable | Num of int | Node of symbol * (term list);;
type atomic = Atom of symbol * (term list);;
type head = Head of atomic;;
type body = Body of atomic list;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;
type goal = Goal of atomic list;; 
type substitution = (variable * term) list;;


(* exceptions *)

exception NOT_UNIFIABLE;;
exception NOT_FOUND;;
exception INVALID_PROGRAM;;
exception NOT_POSSIBLE;;

(* basic functions required *)

let rec mem a l = 
  match l with
    [] -> false
  | h::tl -> (a=h) || (mem a tl)
;;

let rec map fn l = 
  match l with
    [] -> []
  | h::tl -> (fn h)::(map fn tl)
;;

let rec fold_left fn acc l = 
  match l with
    [] -> acc
  | h::tl -> fold_left fn (fn acc h) tl
;;

let rec union l1 l2 =
  match l1 with
    [] -> l2
  | h::tl -> if (mem h l2) then (union tl l2) else h::(union tl l2)
;;

let rec join l1 l2 = 
  match l1 with
    [] -> []
  | h::tl -> (h, (List.hd l2))::(join tl (List.tl l2))
;;



