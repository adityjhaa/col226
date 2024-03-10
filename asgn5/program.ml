type symbol = (string * int);;
type signature = symbol list;;

type tree = V of string | C of { node: symbol ; children: tree list };;

type substitution = (string * tree) list

let check_sig (sign : signature ) : bool =
  let rec check_dup l =
    match l with
    | [] -> false
    | a::t -> 
      if (List.mem a t) then true
      else check_dup t
  in
  let rec check_neg l =
    match l with
    | [] -> false
    | a::t -> 
      if a<0 then true
      else check_neg t
  in 
  not (check_dup (List.map fst sign)) && not (check_neg (List.map snd sign)) 
;;

let rec get_arity (sign : signature) (x: symbol) : int =
  match sign with
  | [] -> 0
  | (a, i)::t -> 
    if a=(fst x) then i 
    else get_arity t x
;;

let rec wftree (t: tree) (sign: signature) : bool = 
  match t with
  | V _ -> true
  | C {node = n; children = clist } ->
    let i = get_arity sign n in
    List.length clist = i && List.for_all (fun c -> wftree c sign) clist
;;

let rec ht (t: tree) : int =
  match t with
  | V _ -> 0
  | C {node = n ; children = clist} -> 
    1 + List.fold_left (fun acc tx -> max acc (ht tx)) 0 clist
;;

let rec size (t: tree) :int =
  match t with
  | V _ -> 1
  | C {node = n ; children = clist} -> 
    1 + List.fold_left (fun acc tx -> acc + (size tx)) 0 clist 
;;

let vars (t: tree) : string list =
  let rec vars_helper ty acc =
    match ty with
    | V x -> x::acc
    | C {node = n ; children = clist} -> 
      List.fold_left(fun acc tx -> vars_helper tx acc) acc clist
  in
    List.sort_uniq compare (vars_helper t [])
;;

let rec mirror (t:tree) :tree =
  match t with
  | V x -> V x
  | C {node = n ; children = clist} -> 
    C {node = n ; children = List.rev (List.map mirror clist)}
;;

let rec check_subst (subst: substitution) : bool =
  let vars = List.map fst subst in
  let rec check_duplicates s acc =
    match s with
    | [] -> true
    | a::t ->
        if List.mem a acc then false
        else check_duplicates t (a::acc)
  in
  check_duplicates vars []
;;

let rec subst_tree (t : tree) (sub : substitution) : tree =
  match t with
  | V x ->
    (try List.assoc x sub with Not_found -> V x)
  | C { node; children } ->
    C { node; children = List.map (fun t -> subst_tree t sub) children }
;;

let subst (sub : substitution) (t: tree) : tree =
  subst_tree t sub
;;

(* The basic implementation *)

(* let compose_subst (s1:substitution) (s2:substitution) (t:tree) : tree =
  subst s2 (subst s1 t)
;; *)

let compose_subst (s1: substitution) (s2: substitution) (t: tree) : tree =
  let rec helper t =
    match t with
    | V x -> 
      (try List.assoc x s1 with Not_found -> 
        (try List.assoc x s2 with Not_found -> V x))
    | C {node; children} -> 
      C {node; children = List.map helper children}
  in
  helper t
;;
