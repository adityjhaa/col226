type symbol = string;;
type arity = int;;
type signature = (symbol * arity) list;;

type tree = V of string | C of { node: symbol ; children: tree list };;

let check_sig (sign : signature ) : bool =
  let rec check_mem x l =
    match l with
    | [] -> false
    | a::t -> if a = x then true 
      else check_mem x t
  in
  let rec check_dup l =
    match l with
    | [] -> false
    | a::t -> if (check_mem a t) then true
      else check_dup t
  in
  let rec check_neg l =
    match l with
    | [] -> false
    | a::t -> if a<0 then true
      else check_neg t
  in 
  not (check_dup (List.map fst sign)) && not (check_neg (List.map snd sign)) 
;;

let rec get_arity sign x =
  match sign with
  | [] -> 0
  | (a, i)::t -> 
    if a=x then i 
    else get_arity t x
;;

let rec wftree (t: tree) (sign: signature) : bool = 
  match t with
  | V _ -> true
  | C {node = n; children = clist } ->
    let i = get_arity sign n in
    List.length clist = i && List.for_all (fun c -> wftree c sign) clist
;;



