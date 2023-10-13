
(**********************Question 1.1*********************)


(**********************Question 1.2 ********************)

let decomposition num =
  if num = 0 then [false]
  else
  let rec aux num acc =
    if num <= 0 then acc
    else let modulo = num mod 2 in 
      if modulo = 1 then aux (num/2) (true :: acc)
      else aux (num/2) (false :: acc)
    in List.rev (aux num [])

(**********************Question 1.3 ********************)

let completion couple =
  let l,t = couple in
  let rec aux l t acc =
    if t = 0 then acc
    else match l with
      | [] -> aux l (t-1) (false::acc)
      | hd :: tl -> aux tl (t-1) (hd::acc)
  in List.rev (aux l t []) 

  (**    test    ****)


(**********************Question 1.4 ********************)


let composition lb = 
  let rec aux lb acc =
    match lb with
    | [] -> acc
    | hd :: tl -> if hd = true then aux tl (acc*2+1)
                  else aux tl (acc*2)
  in aux (List.rev lb) 0


(***test**)
let listbool = [false;true;true];;
composition listbool;;

  
(**********************Question 1.5 ********************)

let table x n = completion ((decomposition x),n);;

(*test*)
table 15 3;;
table 12 4;;
table 5 8;;

(**********************Question 1.6 ********************)

(*récursive*)
let rec pow a n =
  if n = 0 then 1
  else a* (pow a (n-1))

  (*récursive terminale*)
let pow_rec a n =
  let rec aux n acc=
    match n with
    | 0 -> acc  
    | _ -> aux (n-1) (a*acc)
    in aux n 1

let genAlea n = 
  let rec aux n acc  =
    if n < 0 then acc
    else if n < 64 then (decomposition (Int64.to_int (Random.int64 (Int64.of_int (pow 2 n))))) :: acc
    else aux (n-64) ((decomposition  (Int64.to_int (Random.int64 Int64.max_int)))::acc)
  in aux n [];;

  (** test **)
  genAlea 0;;
  genAlea 64;;
  genAlea 65;;
  genAlea 100;;

(**********************Question 2.7 ********************)

type decision_binary_tree =
  | Leaf of bool
  | Node of int * decision_binary_tree * decision_binary_tree


(**********************Question 2.8 ********************)

let rec log2_depth n =
  if n <= 0 then failwith "n <= 0"
  else if n = 1 || n = 2 then 1
  else 1 + log2_depth ((n+1)/2)

(** test: 
    log2 1 = 1; log2 2 = 1; log2 3= 2; log2 4= 2; log2 5 = 3; log2 8 = 3; log2 9 = 4
**)

let depth_arbre lb = log2_depth (List.length lb)


let cons_chemin_list lb =
  let rec aux lb n acc =
    match lb with
    | [] -> acc
    | hd :: tl -> aux tl (n+1) (((decomposition n),hd)::acc)
    in List.rev (aux lb 0 [])



  let cons_vide_arbre n =
    




let cons_arbre lb = 
  let chemin_bool = (cons_chemin_list lb) in
  let rec aux chemin_bool acc=
    match chemin_bool with
    


    



  (**
  match lb with
  | [] -> failwith "Empty liste boolean"
  | [x] -> 
  | hd :: tl ->
*)




