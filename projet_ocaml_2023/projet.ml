(**********************Question 1.1*********************)
type grand_entier = int64 list


(**********************Question 1.2 ********************)
let decomposition num =
  if num = 0L then [false]
  else
  let rec aux num acc =
    if num <= 0L then acc
    else let modulo = Int64.rem num 2L in 
      if modulo = 1L then aux (Int64.div num 2L) (true :: acc)
      else aux (Int64.div num 2L) (false :: acc)
    in List.rev (aux num [])

let int_100 = decomposition 38L
   
(**    test   **)
let ex_dec = decomposition(38L) (*[false; true; true; false; false; true]*)
(**    test correct! ! **)

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
  (*  test correct! *)

  (*** Question 1.2 version int64 list -> bool list ***)
  let decomposition_liste int_list =
    let rec aux l acc =
      match l with 
      |[]->acc
      |x::[]-> acc @ (decomposition x)
      |hd :: tl -> aux tl (acc @ (completion (decomposition hd, 64)))
    in aux int_list []

    let l_int64 = decomposition_liste [4L; 6L]


(**********************Question 1.4 ********************)

let composition lb = 
  let rec aux lb acc =
    match lb with
    | [] -> acc
    | hd :: tl -> if hd = true then aux tl (Int64.add (Int64.mul acc 2L) 1L)
                  else aux tl (Int64.mul acc 2L)
  in aux (List.rev lb) 0L

(***test**)
let listbool = [false;true;true]
let ex_com1 = composition listbool (* - : int = 6L *)
let ex_com2 = composition [true;true] (*- : int = 3L *)
(*  test correct! *)

(****************  version composition_liste bool list -> int list  ***********************)
let rec sup_prefixe l n =
  match l,n with
  |[], n -> []
  |_, 0 -> l
  |hd::tl, n -> if n > 0 then sup_prefixe tl (n-1) 
          else l

let composition_liste lb = 
  let rec aux lb acc =
    match lb with
    | [] -> List.rev acc
    | hd :: tl -> aux (sup_prefixe lb 64) (composition lb ::acc)
  in aux lb []

(***test**)
let listbool = [false;true;true]
let ex_com1 = composition listbool (* - : int = 6L *)
let ex_com2 = composition [true;true] (*- : int = 3L *)
(*  test correct! *)
 let lb3= [false; true; true; false; false; true; false; false; false; false; false;
 false; false; false; false; false; false; false; false; false; false;
 false; false; false; false; false; false; false; false; false; false;
 false; false; false; false; false; false; false; false; false; false;
 false; false; false; false; false; false; false; false; false; false;
 false; false; false; false; false; false; false; false; false; false;
 false; false; false; false; true]

 let ln=sup_prefixe lb3 64 (*[false;true]*)
 let ex_com3 = composition_liste lb3 (*- : int = [38L;2L]*)

 (****test correct!****)

(**********************Question 1.5 ********************)

let table x n = completion ((decomposition x),n)
(*test*)
let tab1 = table 15L 3  (*true; true; true]*)
let tab2 = table 12L 4  (*[false; false; true; true]*)
let tab3 = table 5L 8   (*[true; false; true; false; false; false; false; false]*)

(**********************Question 1.6 ********************)

(*récursive*)
let rec pow_rec a n = 
  if n = 0 then 1L
  else Int64.mul a (pow_rec a (n-1))

(*récursive terminale*)
let pow_rec_t a n =
  let rec aux n acc=
    match n with
    | 0 -> acc  
    | _ -> aux (n-1) (Int64.mul acc a)
    in aux n 1L


  let genAlea n = 
    let rec aux n acc  =
      if n < 0 then acc
      else if n < 32 then (completion ((decomposition (Random.int64 (pow_rec 2L n))),n)) @ acc
      else if n >= 32 && n < 64 then 
        (completion ((decomposition  (Random.int64 ( (pow_rec 2L 32)))),32)) @ 
        (completion ((decomposition  (Random.int64  (pow_rec 2L (n-32)))),n-32))
        @ acc    
        (** Le code ci-dessus c'est pour résoudre le maximum valeur du type de int **)
      else aux (n-64) (completion (((decomposition (Random.int64 Int64.max_int)),64)) @ acc)
    in aux n []
  
let gen61 = (pow_rec 2L 61)  (*int = 2305843009213693952L*)
let gen62 = (pow_rec 2L 62)  (*int =  4611686018427387904L*)
let gen63 = (pow_rec 2L 63)  (*int = -9223372036854775808L*)
(** test **)
let ex_0 = genAlea 0   (*[]*)
let ex_61 = genAlea 61  (*[false; false; false; true; false; false ;...; true]*)
let ex_62 = genAlea 62  (* [true; false; true; false; false; false; true; false; false; true; false; false;
                           true; false; false; false; true; true; true; true; false; false; true; false;
                           true; true; false; true; false; true; true; false; false; true; true; true;
                           true; false; false; true; false; true; false; true; true; false; false; true;
                           true; false; false; true; false; false; false; true; false; true; false; false;
                           false; true] *)
let ex_63 = genAlea 63  (*[true; false; false; false; false; false; false; false; true; false; true; false;
                          false; true; false; true; false; false; false; false; true; true; true; false;
                          true; false; true; false; false; true; false; false; false; true; true; true;
                          false; false; true; false; true; false; true; false; false; true; false; false;
                          false; true; true; false; true; false; true; false; false; false; false; true;
                          false; true; false]*)
let ex_64 = genAlea 64  (*[true; true; false; true; false; true; false; false; true; true; true; false;
                          true; false; false; true; true; true; false; true; false; false; false; true;
                          false; true; true; true; true; false; false; true; true; true; true; true;
                          false; false; false; true; false; true; false; true; false; false; false; true;
                          true; false; false; false; false; false; false; true; true; false; true; false;
                          false; false; true; false]*)
let ex_100 = genAlea 100  (*[false; false; false; true; false; false;  false; true; false; true; false; false;
                            true; true; true; false; false; true; true; true; true; true; false; true;
                            false; true; true; false; true; true; false; true; true; true; true; false;
                             true; false; false; false; true; true; true; false; true; false; true; false;
                            false; true; false; false; false; false; false; false; false; true; true; true;
                            false; false; true; true; false; true; true; true; true; true; true; false;
                            false; false; false; true; true; false;false; true; true; false; false; true;
                            false; false; true; false; false; false; true; true; true; false; true; true;
                            true; false; true; false]*)
(** test correct!**)                              

(**********************Question 2.7 ********************)
type decision_binary_tree =
  | Leaf of bool
  | Node of int * decision_binary_tree * decision_binary_tree

(**********************Question 2.8 ********************)
let rec log2_arete n =
  if n <= 0 then failwith "n <= 0"
  else if n = 1 || n = 2 then 1
  else 1 + log2_arete ((n+1)/2)

(** test: 
    log2 1 = 1; log2 2 = 1; log2 3= 2; log2 4= 2; log2 5 = 3; log2 8 = 3; log2 9 = 4
**)
(*  test correct! *)

let arete_arbre lb = log2_arete (List.length lb)
let cons_chemin_list lb =
  let length = List.length lb in
  let rec aux lb cpt acc =
    match lb with
    | [] -> acc
    | hd :: tl -> let com = List.rev (completion ( (decomposition (Int64.of_int cpt)), log2_arete length) ) in aux tl (cpt+1) (  (com ,hd ) :: acc)
    in  List.rev (aux lb 0 []) 

(*  test *)
let chemin_list1 = cons_chemin_list [false;false;true]   
(* [([false; false], false); ([false; true], false); ([true; false], true)] *)
let chemin_list2 = cons_chemin_list [true;false;true;true;true]
(* [([false; false; false], true); ([false; false; true], false);([false; true; false], true); ([false; true; true], true);([true; false; false], true)] *)
(*  test correct! *)

let cons_vide_arbre n =
  if n = 0 then failwith "Empty tree"
  else 
    let rec constructor arete =
      if arete = n+1 then Leaf(false)
      else Node (arete, constructor (arete+1),constructor (arete+1))
    in constructor 1

let ex_tree = cons_vide_arbre 2  
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
    | Node(arete,l,r) ->
        match chemin with
        | [] -> Leaf(boolean) 
        | hd :: tl -> 
            if hd = false then Node(arete, inserer (tl,boolean) l,r)
            else  Node(arete,l,inserer (tl,boolean) r)  

let ex_inserer = inserer ([false;false],true) (inserer ([true;false],true) ex_tree )  

(*  test  *)
(* insérer 1 leaf et 3 leaf*)
(* val ex_inserer : decision_binary_tree =
  Node (1, Node (2, Leaf true, Leaf false), Node (2, Leaf true, Leaf false)) *)
(*  test correct! *)

let cons_arbre lb = 
  let chemin_bool_list = cons_chemin_list lb in
  let arete = arete_arbre lb in
  let arbre_init = cons_vide_arbre arete in
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

(*  test correct! *)
let ex_100 =genAlea 100
let arb_100 =cons_arbre ex_100

(********************** test de la question 2.8  --> 25899 ********************************)
let ex_25899 = decomposition 25899L
(* [true; true; false; true; false; true; false; false; true; false; true; false;false; true; true] *)
let dbt_25899 = cons_arbre ex_25899
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

(*  test correct! *)
let lfb_100 = liste_feuilles arb_100


(**********************Question 3.10 ********************)

type node_id_type = GE of int64 list | BL of bool

type graphe_node = {id:node_id_type }

type elements = { 
    entier: node_id_type; 
    node_l: graphe_node ref;
    node_r: graphe_node ref;
    depth: int
}
type liste_deja_vus = elements list

(**********************Question 3.11 ********************)
(** Utilisation de ref := et ! **)
let list_vide = ref []    (** val list_vide : '_weak1 list ref = {contents = []} **)
let _ = list_vide := 1L :: !list_vide  (** - : unit = () **)
let list = list_vide  (** - : int list ref = {contents = [1L]} **)


(** Préparation de la fonction compressionParListe**)

(** 1. liste_feille_to_ge **)
let rec sup_prefix0 l =
  match l with
    | [] -> []
    | hd :: tl -> 
      if hd = 0L then sup_prefix0 tl
      else l 

 (** test **)
 let ex_sup_prefix0 = sup_prefix0 [0L;0L;1L;0L;2L;0L] (*[1L; 0L; 2L; 0L]*)
 (** test correct!! **)
 
 let liste_feuille_to_ge lf =
  let rec aux lf acc =  
    let list_length = List.length lf in   
    match list_length <= 64 with
    | true -> List.rev ( sup_prefix0 ((composition lf) ::acc )) 
    | false -> aux (List.rev (completion ((List.rev lf),(list_length - 64)))) ((composition (completion (lf,64))) ::acc)
   (*** explication:  si length > 64, acc @ les premiers 64 ensuite on la reste sauf que les premiers 64 va continuer la récursive ***)
    in let ret = aux lf [] in
  if ret = [] then [0L]
  else ret
(** test liste_feuille_to_ge  est juste derrière que le test de ge_to_liste_feuille **)
 let lfg = liste_feuille_to_ge lfb_100

(** 2. ge_to_liste_feuille **)
(** decomposition version num_liste **)
let decomposition_ge ge_num_liste =
  let rec aux ge_num_liste acc =
  match ge_num_liste with
    | [] -> acc
    | [x] -> acc @ (decomposition x)
    | hd :: tl ->  aux tl ( acc @ ( completion ((decomposition hd),64))) 
  in aux ge_num_liste []

(** test **)  
let ex_dec_ge1 = decomposition_ge [1L] (*[true]*)
let ex_dec_ge2 = decomposition_ge [2L;1L]
(* [false; true; false; false; false; false; false; false; false; false;   10 éléments chaque ligne
    false;false; false; false; false; false; false; false; false; false;
    false; false;false; false; false; false; false; false; false; false;
    false; false; false;false; false; false; false; false; false; false;
    false; false; false; false;false; false; false; false; false; false; 
    false; false; false; false; false; false;false; false; false; false;
    false; false; false; false; true]  -->65 ème élément  *)

(** test correct **)

let ge_to_liste_feuille ge = liste_feuilles (cons_arbre (decomposition_ge ge))
(** test ge_to_liste_feuille **)
let ex_ge_to_fl1 = ge_to_liste_feuille [2L;1L] 
(* 
[false; true; false; false; false; false; false; false; false; false; 
 false;false; false; false; false; false; false; false; false; false; 
 false; false;false; false; false; false; false; false; false; false; 
 false; false; false;false; false; false; false; false; false; false; 
 false; false; false; false;false; false; false; false; false; false;
 false; false; false; false; false;false; false; false; false; false;
 false; false; false; false; true; false; false; false; false; false; --> true c'est 65 éléments
 false; false; false; false; false; false; false; false; false;false;
 false; false; false; false; false; false; false; false; false;false; 
 false; false; false; false; false; false; false; false; false;false; 
 false; false; false; false; false; false; false; false; false;false;
 false; false; false; false; false; false; false; false; false;false; 
 false; false; false; false; false; false; false; false]  --> 128 élements*)

let ex_ge_to_fl2 = ge_to_liste_feuille [1L] 

(** test liste_feuille_to_ge **)
let length1 = List.length (ge_to_liste_feuille [1L])                 (* 2 *)
let ex_lf_to_ge1 = liste_feuille_to_ge (ge_to_liste_feuille [1L])         (* - : int list = [1L] *)
let length2 = List.length (ge_to_liste_feuille [2L;1L])               (* 128 *)
let ex_lf_to_ge2 = liste_feuille_to_ge (ge_to_liste_feuille [2L;1L])       (* - : int list = [2L;1L] *)
let length3 = List.length (ge_to_liste_feuille [3L;2L;1L])             (* 256 *)
let ex_lf_to_ge3 = liste_feuille_to_ge (ge_to_liste_feuille [3L;2L;1L])     (* - : int list = [3L;2L;1L] *)
let length4 = List.length (ge_to_liste_feuille [4L;3L;2L;1L])           (* 256 *)
let length5 = List.length (ge_to_liste_feuille [5L;4L;3L;2L;1L])         (* 512 *)
let ex_lf_to_ge5 = liste_feuille_to_ge (ge_to_liste_feuille [5L;4L;3L;2L;1L]) (* - : int list = [5L; 4L; 3L; 2L; 1L] *)

 (** test correct **)

(** 3. check si n est la première composante d’un couple stocké dans ListeDejaVus **)

let rec check_ldv n ldv =
  match ldv with
  | [] -> false
  | hd :: tl ->  if  n = hd.entier then true
  else check_ldv n tl

(** Utilisation de type structure et instanciation **)
type test = {
  test1 : int;
  test2 : string;
}
    
let a = {
  test1 = 1;
  test2 = "Hello";
}

let dbt_to_liste_deja_vus dbt =
  let listeDejaVus_ref = ref [
    {entier = BL(true);node_l = ref {id = BL(true)};node_r = ref {id = BL(true)}; depth = -1};
    {entier = BL(false);node_l = ref {id = BL(false)};node_r = ref {id = BL(false)}; depth = -1}   
    ] 
  in
  let rec aux dbt =
    let ge = liste_feuille_to_ge (liste_feuilles dbt) in
    match dbt with
    | Leaf (b) -> failwith "Wont reach here!"
    | Node (d,Leaf(a),Leaf(b)) ->       
    let new_node_l = {id = BL(a)} in
    let new_node_r = {id = BL(b)} in
    let element = {entier = GE(ge); node_l = ref new_node_l; node_r = ref new_node_r; depth = d} in
     listeDejaVus_ref := element :: !listeDejaVus_ref 
    | Node (d,l,r) -> 
    let new_node_l = {id = GE(liste_feuille_to_ge (liste_feuilles l))} in
    let new_node_r = {id = GE(liste_feuille_to_ge (liste_feuilles r))} in
    let element = {entier = GE(ge); node_l = ref new_node_l; node_r = ref new_node_r; depth = d} in
    let _ = listeDejaVus_ref := element :: !listeDejaVus_ref in
    let _ = aux l in aux r
    in let _ = aux dbt in !listeDejaVus_ref

let z_sup ldv_ref e_sup replace=
  let rec aux ldv acc =
   match ldv with
  | [] -> acc
  |({entier = e; node_l = l; node_r = r; depth = d} as hd) :: tl -> 
  if e = e_sup  || e = GE([0L]) then aux tl acc
  else if !l.id = e_sup then aux tl ({entier = e;node_l = replace;node_r = r;depth=d} :: acc)
  else if !r.id = e_sup || !r.id = GE([0L]) then aux tl ({entier = e;node_l = l;node_r = replace;depth=d} :: acc)
  else aux tl (hd::acc)
  in let ret = aux !ldv_ref []  in ldv_ref := ret

let rec maj_regle_Z ldv_ref =
  match !ldv_ref with
  | [] -> ()
  | {entier = e; node_l = l; node_r = r; depth = d} :: tl -> 
    if !r.id = BL(false) then
    let _ = z_sup ldv_ref e l in maj_regle_Z (ref tl)
    else maj_regle_Z (ref tl)

(* test *)
let a = ref [{entier = GE [0L]; node_l = {contents = {id = BL false}};
  node_r = {contents = {id = BL false}}; depth = 4};
  {entier = GE [3L]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL true}}; depth = 4}
  ;{entier = GE [0L]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL false}}; depth = 4};
  {entier = GE [2L]; node_l = {contents = {id = GE [2L]}};
  node_r = {contents = {id = GE [0L]}}; depth = 3};
  {entier = GE [0L]; node_l = {contents = {id = BL false}};
    node_r = {contents = {id = BL false}}; depth = 4};
    {entier = GE [2L]; node_l = {contents = {id = GE [2L]}};
node_r = {contents = {id = GE [0L]}}; depth = 3};
{entier = BL false; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL false}}; depth = -1}
]
let f = maj_regle_Z a
let b = !a


let maj_regle_M ldv_ref =
  let reste = ref [] in
  let supp = ref [] in
  let ldv = !ldv_ref in 
  let rec aux ldv =
  match ldv with
  | [] -> let _ = ldv_ref := !reste in ()
  | hd :: tl ->
    if List.mem hd.entier !supp then aux tl
    else
    let rec aux2 tl =
    match tl with
    | [] -> reste := hd :: !reste
    |  hd2 :: tl2 ->
      if hd.entier = hd2.entier then let _ = if not (List.mem hd.entier !supp) then  supp := hd2.entier :: !supp  in aux2 tl2
      else aux2 tl2
    in  let _ = aux2 tl in aux tl
  in aux ldv 

let ge1_to_bltrue ldv_ref =
  let rec aux ldv acc =
   match ldv with
  | [] -> acc
  |({entier = e; node_l = l; node_r = r; depth = d} as hd) :: tl -> 
  if !l.id = GE[1L] then aux tl ({entier = e;node_l = ref {id =BL(true)};node_r = r;depth=d} :: acc)
  else if !r.id = GE[1L] || !r.id = GE([0L]) then aux tl ({entier = e;node_l = l;node_r = ref {id =BL(true)};depth=d} :: acc)
  else aux tl (hd::acc)
  in let ret = aux !ldv_ref []  in ldv_ref := ret

let compressionParListe arbre_decision =
  let ldv_ref = ref (dbt_to_liste_deja_vus arbre_decision) in
  let _ = maj_regle_Z ldv_ref in let _ = maj_regle_M ldv_ref in
  let _ = ge1_to_bltrue ldv_ref in
  !ldv_ref

(** test **)
let ex_25899 = decomposition 25899L
let dbt_25899 = cons_arbre ex_25899
let test_compression_par_liste_25899 =  compressionParListe dbt_25899 

(**********************Question 3.12 ********************)

(** Utilisation de ficher en ocaml **)
let file_test = "./test.dot"

let () = 
  let message = "hello world\ntest\n" in
  let oc =open_out file_test in
  Printf.fprintf oc "%s\n" message;
  close_out oc

(** L'exemple est dans le ficher test.dot **)

(** L'excercice ci-dessous c'est pour ge < 2 pow 64  **)
let ge_to_int ge=
  match ge with
  | GE (il) ->
    (match il with
    | [] -> failwith "no number" 
    | hd :: tl ->  hd)
  | _ -> failwith "not ge it's bl"

let bool_to_string b =
  if b then "true"
  else "false"

let test_il_to_s = ge_to_int (GE[3L;4L;2L;1L]) (*3 correct*)
 
let ge_to_string ge =
  let rec loop ge aux =
    match ge with
    | GE (il) ->
      (match il with
      | [] -> failwith "no number" 
      |x::[] -> aux^(Int64.to_string x)
      | hd :: tl -> loop (GE tl) (aux ^ Int64.to_string  hd ^"_")
      )
    | _ -> failwith "not ge it's bl"
      in loop ge "list_"

  let a =ge_to_string (GE[3L;4L;2L;1L])

  let node_id_to_string node_id =
    match node_id with
     | BL(b) -> bool_to_string b
     | GE (ge) -> ge_to_string node_id

     let b =node_id_to_string (GE[3L;4L;2L;1L]) (*list_3_4_2_1*)

(** l'implémentation de sémantique de dot **)
let cons_graphe_dot_string_liste arbre_de_decision = 
  let elements_liste = compressionParListe arbre_de_decision  in
  let graphe_string_liste = ref [] in
  let rec aux el =
    match el with
    | [] -> !graphe_string_liste
    | hd :: tl -> 
    if hd.entier = BL(true) || hd.entier = BL(false) then aux tl
    else
    let _ = graphe_string_liste :=  (** On peut faire exactement la même chose que figure 2,
         juste remplacer le grand entier par le depth, mais je préfère ge parce que c'est distinct **)
    ((node_id_to_string hd.entier) ^ " -- " ^ node_id_to_string (!(hd.node_l).id) ^ " [style=dotted]") :: 
    ((node_id_to_string hd.entier) ^ " -- " ^ node_id_to_string (!(hd.node_r).id)) :: 
    !graphe_string_liste 
    in aux tl
  in aux elements_liste

  let num = [64444L;86555L;3L]
  let bl_num = decomposition_liste num
  let arb_num = cons_arbre bl_num
  let test_num = cons_graphe_dot_string_liste arb_100

let test_cons_graphe_dot_string_liste = cons_graphe_dot_string_liste arb_num

(** Le principale de construction d'un ficher dot **)    
let file_graphe = "./graphe_par_list.dot"
let dot fichier arbre_de_decision = 
  let _ = () in   
  let head = "graph {" in
  let tail = "}" in
  let s_list = cons_graphe_dot_string_liste arbre_de_decision in
    let oc = open_out file_graphe in
    let _ = Printf.fprintf oc "%s\n" head in (*graph {*)
    let rec aux s_list =
      match s_list with
      | [] -> ()
      | hd :: tl -> let _ = Printf.fprintf oc "%s\n" hd in aux tl
    in let _ = aux s_list in
    let _ = Printf.fprintf oc "%s" tail in (*}*)
    close_out oc
    
let test_graphe = dot file_graphe dbt_25899
     
(**********************Question 3.13 ********************)
(* On a déjà fait cette question quand on teste la question 2.8 *)
let ex_25899 = decomposition 25899L
(* [true; true; false; true; false; true; false; false; true; false; true; false;false; true; true] *)
let dbt_25899 = cons_arbre ex_25899
(* val dbt_25899 : decision_binary_tree/2 =
Node (1,
  Node (2,
  Node (3, Node (4, Leaf true, Leaf true), Node (4, Leaf false, Leaf true)),
  Node (3, Node (4, Leaf false, Leaf true), Node (4, Leaf false, Leaf false))),
  Node (2,
  Node (3, Node (4, Leaf true, Leaf false), Node (4, Leaf true, Leaf false)),
  Node (3, Node (4, Leaf false, Leaf true), Node (4, Leaf true, Leaf false)))) *)

  (** C'est exactement la même arbre que la figure 1 dans l'annoncé**)

(**********************Question 3.14 ********************)

(* GraphViz Pocket Reference compressionParListe *)
(* https://graphs.grevian.org/graph/4854942932140032 *)


(**********************Question 4.15 ********************)
type arbre_deja_vus =
  | Noeud of elements * arbre_deja_vus * arbre_deja_vus
  | Feuille


(**********************Question 4.16 ********************)
(* 
if once_false then
let new_node_l = {id = BL(a)} in
let new_node_r = {id = BL(b)} in
let element = {entier = GE(ge); node_l = ref new_node_l; node_r = ref new_node_r; depth = d} in
  Noeud (element,Feuille,Feuille)
| else
  let element_false = {entier = BL(false); node_l = ref {id = BL(false)}; node_r = ref {id= BL(false)}; depth = -1} in
  Noeud (element,element_false,Feuille) *)

  let  add_true_false adv =
    let f_ref = ref false in
    let rec aux adv =
    match adv with
    | Feuille -> 
      if not !f_ref then 
      let _ = f_ref := true in
      Noeud({entier = BL(false); node_l = ref {id = BL(false)}; node_r = ref {id = BL(false)}; depth = -1},Noeud({entier = BL(true); node_l = ref {id = BL(true)}; node_r = ref {id = BL(true)}; depth = -1},Feuille,Feuille),Feuille)
      else Feuille
    | Noeud(elt,l,r) -> Noeud(elt,aux l, aux r)
    in aux adv

let dbt_to_arbre_deja_vus dbt =
  let rec aux dbt = 
  let ge = liste_feuille_to_ge (liste_feuilles dbt) in
    match dbt with
  | Leaf (b) -> failwith "Wont reach here!"
  | Node (d,Leaf(a),Leaf(b)) -> 
    let new_node_l = {id = BL(a)} in
    let new_node_r = {id = BL(b)} in
    let element = {entier = GE(ge); node_l = ref new_node_l; node_r = ref new_node_r; depth = d} in
      Noeud (element,Feuille,Feuille)
  | Node (d,l,r) -> 
    let new_node_l = {id = GE(liste_feuille_to_ge (liste_feuilles l))} in
    let new_node_r = {id = GE(liste_feuille_to_ge (liste_feuilles r))} in
    let element = {entier = GE(ge); node_l = ref new_node_l; node_r = ref new_node_r; depth = d} in
    Noeud(element, aux l , aux r)
  in let new_dbt = aux dbt in (*add_true_false new_dbt*) new_dbt

let test_25899_adv = dbt_to_arbre_deja_vus dbt_25899 

(* Noeud
({entier = GE [25899]; node_l = {contents = {id = GE [43]}};
  node_r = {contents = {id = GE [101]}}; depth = 1},
Noeud
 ({entier = GE [43]; node_l = {contents = {id = GE [11]}};
   node_r = {contents = {id = GE [2]}}; depth = 2},
 Noeud
  ({entier = GE [11]; node_l = {contents = {id = GE [3]}};
    node_r = {contents = {id = GE [2]}}; depth = 3},
  Noeud
   ({entier = GE [3]; node_l = {contents = {id = BL true}};
     node_r = {contents = {id = BL true}}; depth = 4},
   Feuille, Feuille),
  Noeud
   ({entier = GE [2]; node_l = {contents = {id = BL false}};
     node_r = {contents = {id = BL true}}; depth = 4},
   Feuille, Feuille)),
 Noeud
  ({entier = GE [2]; node_l = {contents = {id = GE [2]}};
    node_r = {contents = {id = GE [0]}}; depth = 3},
  Noeud
   ({entier = GE [2]; node_l = {contents = {id = BL false}};
     node_r = {contents = {id = BL true}}; depth = 4},
   Feuille, Feuille),
  Noeud
   ({entier = GE [0]; node_l = {contents = {id = BL false}};
     node_r = {contents = {id = BL false}}; depth = 4},
   Feuille, Feuille))),
Noeud
 ({entier = GE [101]; node_l = {contents = {id = GE [5]}};
   node_r = {contents = {id = GE [6]}}; depth = 2},
 Noeud
  ({entier = GE [5]; node_l = {contents = {id = GE [1]}};
    node_r = {contents = {id = GE [1]}}; depth = 3},
  Noeud
   ({entier = GE [1]; node_l = {contents = {id = BL true}};
     node_r = {contents = {id = BL false}}; depth = 4},
   Feuille, Feuille),
  Noeud
   ({entier = GE [1]; node_l = {contents = {id = BL true}};
     node_r = {contents = {id = BL false}}; depth = 4},
   Feuille, Feuille)),
 Noeud
  ({entier = GE [6]; node_l = {contents = {id = GE [2]}};
    node_r = {contents = {id = GE [1]}}; depth = 3},
  Noeud
   ({entier = GE [2]; node_l = {contents = {id = BL false}};
     node_r = {contents = {id = BL true}}; depth = 4},
   Feuille, Feuille),
  Noeud
   ({entier = GE [1]; node_l = {contents = {id = BL true}};
     node_r = {contents = {id = BL false}}; depth = 4},
   Feuille, Feuille)))) *)

  (** 15 noeuds sans BL **)
  (* test correct! *)

let rec is_in_tree ge tree =
  match tree with
  | Feuille -> false
  | Noeud(elt,l,r) -> 
    if elt.entier = ge then true
    else is_in_tree ge l || is_in_tree ge r

(* test *)
let ex_tree = Noeud({entier = GE([1L]); node_l = ref {id = BL(true)}  ; node_r =  ref {id = BL(false)}; depth = 1},Feuille,Feuille)
let ex_test1 = is_in_tree (GE[1L]) ex_tree (*true*)
let ex_test2 = is_in_tree (GE[2L]) ex_tree (*false*)
(* test correct! *)

let inserer_node_in_tree node tree =
  match node.entier with
  | BL(b) -> tree
  | GE(ge) ->
    if is_in_tree node.entier tree then tree
    else 
    let feuille_liste = ge_to_liste_feuille ge in
    let rec aux tree bl=
    match tree with
    | Feuille -> Noeud(node,Feuille,Feuille)
    | Noeud(elt,l,r) -> match bl with
                        | [] -> Noeud(node,Feuille,Feuille)
                        | hd :: tl -> if hd = false then Noeud(elt, aux l tl,r)
                                      else Noeud(elt,l, aux r tl)
    in aux tree feuille_liste

(* test *)
let ex_test3 = inserer_node_in_tree {entier = GE([1L]); node_l = ref {id = BL(true)}  ; node_r =  ref {id = BL(false)}; depth = 1} ex_tree
let ex_test4 = inserer_node_in_tree {entier = GE([2L]); node_l = ref {id = BL(false)}  ; node_r =  ref {id = BL(true)}; depth = 1} ex_tree
(* Noeud
({entier = GE [1]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL false}}; depth = 1},
Noeud
 ({entier = GE [2]; node_l = {contents = {id = BL false}};
   node_r = {contents = {id = BL true}}; depth = 1},
 Feuille, Feuille),
Feuille) *)

(* test correct! *)
let maj_node_in_tree_l node tree node_l =
  match node.entier with
  | BL(b) -> tree
  | GE(ge) ->
    let rec aux tree =
    match tree with
    | Feuille -> Feuille
    | Noeud(elt,l,r) -> if node.entier = elt.entier then  Noeud({entier = node.entier; node_l = node_l; node_r = elt.node_r; depth = elt.depth},aux l, aux r)
    else Noeud(elt,aux l, aux r)
    in aux tree 

  (* test *)
  let ex_maj_l = maj_node_in_tree_l {entier = GE([1L]); node_l = ref {id = BL(true)}  ; node_r =  ref {id = BL(false)}; depth = 1} ex_test4  (ref {id = GE[2L]})
  (* test correct! *)

  let maj_node_in_tree_r node tree node_r =
    match node.entier with
    | BL(b) -> tree
    | GE(ge) ->
      let rec aux tree =
      match tree with
      | Feuille -> Feuille
      | Noeud(elt,l,r) -> if node.entier = elt.entier then  Noeud({entier = node.entier; node_l = elt.node_l; node_r = node_r; depth = elt.depth},aux l, aux r)
      else Noeud(elt,aux l, aux r)
      in aux tree 

(* test *)
let ex_maj_r = maj_node_in_tree_r {entier = GE([1L]); node_l = ref {id = BL(true)}  ; node_r =  ref {id = BL(false)}; depth = 1} ex_test4  (ref {id = GE[2L]})
(* test correct! *)

(* let rec maj_l_elt_in_tree ge tree l  *)

(**********************Question 4.17 ********************)

let get_noeud_par_chemin adv blist =
  let rec aux adv blist = 
   match adv , blist with
   | Feuille, _ -> Feuille
   | noeud , [] -> noeud
   | Noeud(elt,l,r) , hd ::tl -> if hd = false then aux l tl
   else aux r tl
   in aux adv blist

let regle_M adv =
  let ret_ref = ref Feuille in 
  let left = true in
  let rec aux courant left=
    match courant with
    | Feuille -> !ret_ref
    | Noeud(elt,l,r) -> 
      if is_in_tree elt.entier !ret_ref then let _ = aux l left in aux r (not left)
      else if left then let _ = ret_ref := Noeud(elt,!ret_ref,Feuille) in let _ = aux l (left) in aux r (not left)
      else let _ = ret_ref := Noeud(elt,Feuille,!ret_ref) in let _ = aux l (left) in aux r (not left)
  in aux adv left
   
(* test M*)  
let test_M = regle_M test_25899_adv
let test_M_ref = ref (regle_M test_25899_adv)   
let test_M = !test_M_ref 
(* 
val test_M : arbre_deja_vus =
  Noeud
   ({entier = GE [6]; node_l = {contents = {id = GE [2]}};
     node_r = {contents = {id = GE [1]}}; depth = 3},
   Noeud
    ({entier = GE [1]; node_l = {contents = {id = BL true}};
      node_r = {contents = {id = BL false}}; depth = 4},
    Feuille,
    Noeud
     ({entier = GE [5]; node_l = {contents = {id = GE [1]}};
       node_r = {contents = {id = GE [1]}}; depth = 3},
     Feuille,
     Noeud
      ({entier = GE [101]; node_l = {contents = {id = GE [5]}};
        node_r = {contents = {id = GE [6]}}; depth = 2},
      Feuille,
      Noeud
       ({entier = GE [0]; node_l = {contents = {id = BL false}};
         node_r = {contents = {id = BL false}}; depth = 4},
       Noeud
        ({entier = GE [2]; node_l = {contents = {id = BL false}};
          node_r = {contents = {id = BL true}}; depth = 4},
        Feuille,
        Noeud
         ({entier = GE [3]; node_l = {contents = {id = BL true}};
           node_r = {contents = {id = BL true}}; depth = 4},
         Noeud
          ({entier = GE [11]; node_l = {contents = {id = GE [3]}};
            node_r = {contents = {id = GE [2]}}; depth = 3},
          Noeud
           ({entier = GE [43]; node_l = {contents = {id = GE [11]}};
             node_r = {contents = {id = GE [2]}}; depth = 2},
           Noeud
            ({entier = GE [25899]; node_l = {contents = {id = GE [43]}};
              node_r = {contents = {id = GE [101]}}; depth = 1},
            Feuille, Feuille),
           Feuille),
          Feuille),
         Feuille)),
       Feuille)))),
   Feuille)
   *)


let to_list adv =
  let rec aux adv acc =
  match adv with
  | Feuille ->  acc
  | Noeud(elt,l,r) -> aux r (aux l  (elt :: acc))
  in aux adv []

  let a = to_list test_M
  let origin = to_list test_25899_adv

let supp_in_list l ge =
  let rec aux l acc =
    match l with
  | [] -> acc
  | hd ::tl -> if hd.entier = ge then aux tl acc
  else aux tl (hd :: acc)
  in aux l []

let to_adv list =
  let rec aux list acc boolean=
    match list with
    | [] -> acc
    | hd :: tl ->
    if not boolean then aux tl (Noeud(hd,acc,Feuille)) true
    else aux tl (Noeud(hd,Feuille,acc)) false
    in aux list Feuille false

let b =  to_adv a
let test_supp_in_list = supp_in_list a (GE[0L]) 

let c = to_adv test_supp_in_list

let d = ref origin
let f = maj_regle_Z d
let e = !d


let compressionParArbre arbre_decision =
  let adv_ref = ref (dbt_to_arbre_deja_vus arbre_decision) in
  let list_ref = ref (to_list !adv_ref) in
  let _  = maj_regle_Z list_ref in let _ = ge1_to_bltrue list_ref in  let  _ =  adv_ref := to_adv (!list_ref) in let ret = regle_M !adv_ref in
  ret

let finish = compressionParArbre dbt_25899

let cons_graphe_dot_string_arbre arbre_de_decision = 
  let elements_arbre = compressionParArbre arbre_de_decision  in
  let graphe_string_liste = ref [] in
  let rec aux el =
    match el with
    | Feuille -> !graphe_string_liste
    | Noeud(elt,l,r) -> 
    if elt.entier = BL(true) || elt.entier = BL(false) then let _ = aux l in aux r
    else
    let _ = graphe_string_liste :=  (** On peut faire exactement la même chose que figure 2,
         juste remplacer le grand entier par le depth, mais je préfère ge parce que c'est distinct **)
    ((node_id_to_string elt.entier) ^ " -- " ^ node_id_to_string (!(elt.node_l).id) ^ " [style=dotted]") :: 
    ((node_id_to_string elt.entier) ^ " -- " ^ node_id_to_string (!(elt.node_r).id)) :: 
    !graphe_string_liste 
    in let _ = aux l in aux r
  in aux elements_arbre

let test_cons_graphe_dot_string_arbre = cons_graphe_dot_string_arbre dbt_25899


(** Le principale de construction d'un ficher dot **)    
let file_graphe = "./graphe_par_arbre.dot"
let dot fichier arbre_de_decision = 
  let _ = () in   
  let head = "graph {" in
  let tail = "}" in
  let s_list = cons_graphe_dot_string_arbre arbre_de_decision in
    let oc = open_out file_graphe in
    let _ = Printf.fprintf oc "%s\n" head in (*graph {*)
    let rec aux s_list =
      match s_list with
      | [] -> ()
      | hd :: tl -> let _ = Printf.fprintf oc "%s\n" hd in aux tl
    in let _ = aux s_list in
    let _ = Printf.fprintf oc "%s" tail in (*}*)
    close_out oc
    
let test_graphe = dot file_graphe dbt_25899

(* GraphViz Pocket Reference compressionParArbre*)
(* https://graphs.grevian.org/graph/6211502614773760 *)


(****************** Question 6.20 *******************)

let test_mesure n =  cons_arbre (genAlea n)

let test_2_pow_n = test_mesure 1024
let time_compression_Liste () =
  let _ = compressionParListe test_2_pow_n in
  ()
let time_compression_Arbre () =
  let _ = compressionParArbre test_2_pow_n in
  ()
(*******************  mesure time *******************)
let measure_time f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  (result, elapsed_time)
let () = 
  let (_, elapsed_time) = measure_time time_compression_Liste in
  let _ = Printf.printf "Function compressionParListe took %f seconds to run.\n" elapsed_time in
  let (_, elapsed_time) = measure_time time_compression_Arbre in
  Printf.printf "Function compressionParArbre took %f seconds to run.\n" elapsed_time
(*******************  mesure space *******************)

let measure_memory_use_by_list f =
    let open Gc in
    let initial_stats = quick_stat () in
    let _ = f () in 
    let after_stats = quick_stat () in
    let memory_used = after_stats.minor_words -. initial_stats.minor_words in
    Printf.printf "Approximate LISTE memory used: %f words\n" memory_used

let () =
    measure_memory_use_by_list time_compression_Liste

let measure_memory_use_by_tree f =
  let open Gc in
  let initial_stats = quick_stat () in
  let _ = f () in 
  let after_stats = quick_stat () in
  let memory_used = after_stats.minor_words -. initial_stats.minor_words in
  Printf.printf "Approximate ARBRE memory used: %f words\n" memory_used

let () =
  measure_memory_use_by_tree time_compression_Arbre  

(*************   mesure taux compression  *************)
let count_node dbt =
  let rec aux dbt acc =
  match dbt with
  | Leaf(b) -> acc
  | Node(_,l,r) -> aux r (aux l (acc+1))
  in aux dbt 0
let count_noeud ele_list =
  let rec aux ele_list acc =
    match ele_list with
    | [] -> acc
    | hd :: tl -> aux tl (acc+1)
    in aux ele_list 0

let taux_compression dbt =
  let origin_count = count_node dbt in
  let compress_count = count_noeud (compressionParListe dbt) in
  (float_of_int compress_count) /.  (float_of_int origin_count)
let () = let a = taux_compression test_2_pow_n in
Printf.printf "Compreesion taux = %f%%.\n" (a*.100.)
