
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
    in List.rev (aux num []);;

(**    test   **)
let ex_dec = decomposition(38);; (*[false; true; true; false; false; true]*)
(**    test correcte ! **)

(**********************Question 1.3 ********************)

let completion couple =
  let l,t = couple in
  let rec aux l t acc =
    if t = 0 then acc
    else match l with
      | [] -> aux l (t-1) (false::acc)
      | hd :: tl -> aux tl (t-1) (hd::acc)
  in List.rev (aux l t []) 

  (****    test  ****)
  let com_1 = completion([false; true; true; false; false; true], 4)   (*[false; true; true; false]*)
  let com_2 =  completion([false; true; true; false; false; true], 8)  (*[false; true; true; false; false; true; false; false]*)

  (*  test correcte *)

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
composition listbool;; (* - : int = 6 *)
(*  test correcte *)


  
(**********************Question 1.5 ********************)

let table x n = completion ((decomposition x),n);;

(*test*)
table 15 3;;  (*true; true; true]*)
table 12 4;;  (*[false; false; true; true]*)
table 5 8;;   (*[true; false; true; false; false; false; false; false]*)

(**********************Question 1.6 ********************)

(*récursive*)
let rec pow_rec a n =
  if n = 0 then 1
  else a* (pow_rec a (n-1))

(*récursive terminale*)
let pow_rec_t a n =
  let rec aux n acc=
    match n with
    | 0 -> acc  
    | _ -> aux (n-1) (a*acc)
    in aux n 1

let genAlea n = 
  let rec aux n acc  =
    if n < 0 then acc
    else if n < 64 then (decomposition (Int64.to_int (Random.int64 (Int64.of_int (pow_rec 2 n))))) :: acc
    else aux (n-64) ((decomposition  (Int64.to_int (Random.int64 Int64.max_int)))::acc)
  in aux n [];;

  let gen61 = (pow_rec 2 61);;  (*int = 2305843009213693952*)
  let gen62 = (pow_rec 2 62);;  (*int = -4611686018427387904*)

  let gen63 = (pow_rec 2 63);;  (*int = 0*)
  (** test **)
  genAlea 0;;   (*[[false]]*)
  genAlea 61;;  (*[[false; false; false; true; false; false ;...; true]]*)
  genAlea 62;;  (*Exception: Invalid_argument "Random.int64".*)
  genAlea 63;;  (*Exception: Invalid_argument "Random.int64".*)
  genAlea 64;;  (*[[false];
                    [true; true; false; true; false; true; true; false; false; false; false; true;
                    true; false; true; false; false; true; true; false; false; false; false;
                    true; true; false; true; true; true; false; false; false; false; false;
                    false; true; false; false; true; true; false; true; true; false; true; true;
                    false; false; true; false; false; true; true; true; true; true; true; true]]*)

  genAlea 100;;  (* [ [true; false; false; false; true; true; false; false; false; false; false;
                      false; true; true; true; true; true; true; true; false; true; false; true;
                      false; false; true; true; false; false; true; true; false; true];
                              [true; false; false; true; true; false; true; false; false; true; false; true;
                              true; true; true; true; false; true; true; false; false; false; false; false;
                              false; true; true; false; false; false; false; false; false; true; true;
                              true; true; true; true; true; true; false; true; true; true; false; true;
                              true; true; false; true; true; true; false; false; false; true; true; true;
                              true; true]]*)


(*    test :
    Pas complétement correcte,
   En conclusion, 2^62 et 2^63 a déjà dépassé le maximum de type int.
*)

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
(*  test correcte *)

let depth_arbre lb = log2_depth (List.length lb)


let cons_chemin_list lb =
  let length = List.length lb in
  let rec aux lb n acc =
    match lb with
    | [] -> acc
    | hd :: tl -> let com = List.rev (completion ( (decomposition n), log2_depth length) ) in aux tl (n+1) (  (com ,hd ) :: acc)
    in  List.rev (aux lb 0 []) ;;

(*  test *)
let chemin_list1 = cons_chemin_list [false;false;true] ;;   
(* [([false; false], false); ([false; true], false); ([true; false], true)] *)
let chemin_list2 = cons_chemin_list [true;false;true;true;true];;
(* 
[([false; false; false], true); ([false; false; true], false);([false; true; false], true); ([false; true; true], true);([true; false; false], true)] *)

(*  test correcte *)

  let cons_vide_arbre n =
    if n = 0 then failwith "Empty tree"
    else 
      let rec constructor depth =
        if depth = n+1 then Leaf(false)
        else Node (depth, constructor (depth+1),constructor (depth+1))
      in constructor 1

let ex_tree = cons_vide_arbre 2 ;; 
(*val ex_tree : decision_binary_tree =
   Node (1, 
      Node (2, Leaf false, Leaf false), 
      Node (2, Leaf false, Leaf false))*)


let rec inserer chemin_bool arbre =
  let chemin,boolean = chemin_bool in
  if boolean = false then arbre  (*Parce que cons_vide c'est tous false *)
  else
  match arbre with
    | Leaf(_) -> Leaf(boolean)
    | Node(depth,l,r) ->
        match chemin with
        | [] -> Leaf(boolean) 
        | hd :: tl -> 
            if hd = false then Node(depth, inserer (tl,boolean) l,r)
            else  Node(depth,l,inserer (tl,boolean) r)  

let ex_inserer = inserer ([false;false],true) (inserer ([true;false],true) ex_tree )  

(*  test  *)
(* insérer 1 leaf et 3 leaf*)
(* val ex_inserer : decision_binary_tree =
  Node (1, Node (2, Leaf true, Leaf false), Node (2, Leaf true, Leaf false)) *)
(*  test correcte *)

let cons_arbre lb = 
  let chemin_bool_list = cons_chemin_list lb in
  let depth = depth_arbre lb in
  let arbre_init = cons_vide_arbre depth in
  let rec inserer_list chemin_bool_list acc =
    match chemin_bool_list with
    | [] -> acc
    | hd :: tl -> inserer_list tl (inserer hd acc)
    in inserer_list chemin_bool_list arbre_init 

(*  test *)
let ex_list1 = [true;false;true]
let ex_list2 = [true;true;true;true;false;false;false;false;true]

let ex_cons_arbre1 = cons_arbre ex_list1 
(* [true; true; true; true; false; false; false; false; true] *)

let ex_cons_arbre2 = cons_arbre ex_list2 

(* Node (1,
   Node (2,
    Node (3, Node (4, Leaf true, Leaf true), Node (4, Leaf true, Leaf true)),
    Node (3, Node (4, Leaf false, Leaf false),
     Node (4, Leaf false, Leaf false))),
   Node (2,
    Node (3, Node (4, Leaf true, Leaf false), Node (4, Leaf false, Leaf false)),
    Node (3, Node (4, Leaf false, Leaf false),
     Node (4, Leaf false, Leaf false)))) *)

(*  test correcte *)

(********************** test de la question 2.8  --> 25899 ********************************)

let ex_25899 = decomposition 25899;;
(* [true; true; false; true; false; true; false; false; true; false; true; false;false; true; true] *)

let dbt_25899 = cons_arbre ex_25899;;

(* Node (1,
    Node (2,
      Node (3, Node (4, Leaf true, Leaf true), Node (4, Leaf false, Leaf true)),
      Node (3, Node (4, Leaf false, Leaf true), Node (4, Leaf false, Leaf false))),
    Node (2,
      Node (3, Node (4, Leaf true, Leaf false), Node (4, Leaf true, Leaf false)),
      Node (3, Node (4, Leaf false, Leaf true), Node (4, Leaf true, Leaf false)))) *)

      (********************** exactement correcte ********************)
(***********************************************************************************)


(**********************Question 2.9 ********************)

let liste_feuilles arbre =
  let rec aux arbre acc =
    match arbre with
      | Leaf (b) -> b :: acc
      | Node (_,l,r) -> aux r (aux l acc)
  in List.rev (aux arbre [])

let liste_feuilles_25899 = liste_feuilles dbt_25899

(*      test:
    [ true; true; false; true; 
      false; true; false; false; 
      true; false; true; false; 
      false; true; true; false]  correcte!  *)

(*  test correcte *)


(**********************Question 3.10 ********************)







