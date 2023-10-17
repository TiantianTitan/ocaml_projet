
(**********************Question 1.1*********************)
type grand_entier = int list

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

(**    test   **)
let ex_dec = decomposition(38) (*[false; true; true; false; false; true]*)
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
let listbool = [false;true;true]
let ex_com1 = composition listbool (* - : int = 6 *)
let ex_com2 = composition [true;true] (*- : int = 3 *)
(*  test correcte *)


  
(**********************Question 1.5 ********************)

let table x n = completion ((decomposition x),n)
(*test*)
let tab1 = table 15 3  (*true; true; true]*)
let tab2 = table 12 4  (*[false; false; true; true]*)
let tab3 = table 5 8   (*[true; false; true; false; false; false; false; false]*)

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
    else if n < 32 then (decomposition (Int64.to_int (Random.int64 (Int64.of_int (pow_rec 2 n))))) :: acc
    else if n >= 32 && n < 64 then 
      (decomposition (Int64.to_int (Random.int64 (Int64.of_int (pow_rec 2 32))))) :: 
      (decomposition (Int64.to_int (Random.int64 (Int64.of_int (pow_rec 2 (n-32)))))) 
      :: acc    
      (** Le code ci-dessus c'est pour résoudre le maximum valeur du type de int **)
    else aux (n-64) ((decomposition  (Int64.to_int (Random.int64 Int64.max_int)))::acc)
  in aux n []
  

let gen61 = (pow_rec 2 61)  (*int = 2305843009213693952*)
let gen62 = (pow_rec 2 62)  (*int = -4611686018427387904*)
let gen63 = (pow_rec 2 63)  (*int = 0*)
(** test **)
let ex_0 = genAlea 0   (*[[false]]*)
let ex_61 = genAlea 61  (*[[false; false; false; true; false; false ;...; true]]*)
let ex_62 = genAlea 62  (* [[true; false; false; true; false; true; false; true; false; false; true;
                            false; true; true; true; false; false; false; true; false; true; false;
                            true; true; true; false; false; true; true];
                            [true; true; false; false; false; false; true; false; true; false; true;
                            false; false; true; true; false; true; false; true; true; false; true;
                            false; true; true; true; false; false; false; true]] *)
let ex_63 = genAlea 63  (*[[true; true; true; true; false; true; true; false; false; false; false;
                          false; true; true; true; true; false; false; false; false; true; true;
                          true; true; false; false; false; false; true; true; false; true];
                          [true; true; false; false; false; false; true; true; false; false; false;
                          false; false; true; true; true; true; true; false; true; false  ; true; true;
                          true; true; false; true; true; true]]*)
let ex_64 = genAlea 64  (*[[false];[true; true; false; true; false; true; true; false; false; false; false; true;
                          true; false; true; false; false; true; true; false; false; false; false;true; true; false;
                          true; true; true; false; false; false; false; false;false; true; false; false; true; true; 
                          false; true; true; false; true; true; false; false; true; false; false; true; true; true; 
                          true; true; true; true]]*)
let ex_100 = genAlea 100  (* [ [true; false; false; false; true; true; false; false; false; false; false;
                              false; true; true; true; true; true; true; true; false; true; false; true;
                              false; false; true; true; false; false; true; true; false; true];
                              [true; false; false; true; true; false; true; false; false; true; false; true;
                              true; true; true; true; false; true; true; false; false; false; false; false;
                              false; true; true; false; false; false; false; false; false; true; true;
                              true; true; true; true; true; true; false; true; true; true; false; true;
                              true; true; false; true; true; true; false; false; false; true; true; true;
                              true; true]]*)
                              
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
  let rec aux lb cpt acc =
    match lb with
    | [] -> acc
    | hd :: tl -> let com = List.rev (completion ( (decomposition cpt), log2_depth length) ) in aux tl (cpt+1) (  (com ,hd ) :: acc)
    in  List.rev (aux lb 0 []) 

(*  test *)
let chemin_list1 = cons_chemin_list [false;false;true]   
(* [([false; false], false); ([false; true], false); ([true; false], true)] *)
let chemin_list2 = cons_chemin_list [true;false;true;true;true]
(* [([false; false; false], true); ([false; false; true], false);([false; true; false], true); ([false; true; true], true);([true; false; false], true)] *)
(*  test correcte *)

let cons_vide_arbre n =
  if n = 0 then failwith "Empty tree"
  else 
    let rec constructor depth =
      if depth = n+1 then Leaf(false)
      else Node (depth, constructor (depth+1),constructor (depth+1))
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
let ex_25899 = decomposition 25899
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

(*  test correcte *)

(**********************Question 3.10 ********************)

type node = { id: int; next_id: int; depth: int}

type elements = { 
    entier: int list; 
    node: node 
}

type liste_deja_vus = elements list

(**********************Question 3.11 ********************)
(** Utilisation de ref := et ! **)
let list_vide = ref []    (** val list_vide : '_weak1 list ref = {contents = []} **)
let _ = list_vide := 1 :: !list_vide  (** - : unit = () **)
let list = list_vide  (** - : int list ref = {contents = [1]} **)


(** Préparation de la fonction compressionParListe**)

(** 1. liste_feille_to_ge **)
let rec sup_prefix0 l =
  match l with
    | [] -> []
    | hd :: tl -> 
      if hd = 0 then sup_prefix0 tl
      else l 

 (** test **)
 let ex_sup_prefix0 = sup_prefix0 [0;0;1;0;2;0] (*[1; 0; 2; 0]*)
 (** test correcte! **)
 
let liste_feuille_to_ge lf =
  let rec aux lf acc =  
    let list_length = List.length lf in   
    match list_length <= 64 with
    | true -> List.rev ( sup_prefix0 ((composition lf) ::acc )) 
    | false -> aux (List.rev (completion ((List.rev lf),(list_length - 64)))) ((composition (completion (lf,64))) ::acc)
    (*** explication:  si length > 64, acc @ les premiers 64 ensuite on la reste sauf que les premiers 64 va continuer la récursive ***)
  in aux lf []
(** test liste_feuille_to_ge  est juste derrière que le test de ge_to_liste_feuille **)

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
let ex_dec_ge1 = decomposition_ge [1] (*[true]*)
let ex_dec_ge2 = decomposition_ge [2;1]
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
let ex_ge_to_fl1 = ge_to_liste_feuille [2;1] 
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

let ex_ge_to_fl2 = ge_to_liste_feuille [1] 

(** test liste_feuille_to_ge **)
let length1 = List.length (ge_to_liste_feuille [1])                 (* 2 *)
let ex_lf_to_ge1 = liste_feuille_to_ge (ge_to_liste_feuille [1])         (* - : int list = [1] *)
let length2 = List.length (ge_to_liste_feuille [2;1])               (* 128 *)
let ex_lf_to_ge2 = liste_feuille_to_ge (ge_to_liste_feuille [2;1])       (* - : int list = [2;1] *)
let length3 = List.length (ge_to_liste_feuille [3;2;1])             (* 256 *)
let ex_lf_to_ge3 = liste_feuille_to_ge (ge_to_liste_feuille [3;2;1])     (* - : int list = [3;2;1] *)
let length4 = List.length (ge_to_liste_feuille [4;3;2;1])           (* 256 *)
let length5 = List.length (ge_to_liste_feuille [5;4;3;2;1])         (* 512 *)
let ex_lf_to_ge5 = liste_feuille_to_ge (ge_to_liste_feuille [5;4;3;2;1]) (* - : int list = [5; 4; 3; 2; 1] *)

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

(* test *)
let ex_node = {id = 0; next_id = 1;depth = 1} 
let ex_element = { entier = [1;2;3] ; node = ex_node ; } 
let ex_ldv = [ex_element] 
let ex_check1 = check_ldv [1;2;3] ex_ldv  (*true*)
let ex_check2 = check_ldv [1;2] ex_ldv    (*false*)
(* test correcte! *)

(* let child_noeud elem l_ref =
  let l = !l_ref in
  let rec aux l =
  match l with
  | [] -> failwith "no child with the node"
  | hd :: tl -> 
    if hd.entier = elem then hd.node 
    else aux tl
    in aux l *)

 (** test child_noeud**)

(* let ex_ldv_ref = ref [{entier = [1;2]; node = {id = 1}};{entier = [1;2;3]; node = {id = 2}}]
let ex_child_noeud = child_noeud  [1;2] ex_ldv_ref *)

(** not test **)

let maj_noeud elem noeud l_ref =
    let l = !l_ref in
    let rec aux l acc=
      match l with
      | [] -> acc
      | { entier = e ;node = n } :: tl -> 
        if e = elem then aux tl ({entier = e; node = noeud} :: acc)
        else   aux tl ({ entier = e ;node = n } :: acc)
    in let _ = l_ref :=  (aux l []) in ()

(** test_maj_noeud **)
let ex_ldv_ref = ref [{entier = [1;2]; node = {id = 1;next_id = 2; depth = 1}};{entier = [1;2;3]; node = {id = 2;next_id =3;depth = 2}}]
let ex_maj_noeud = maj_noeud [1;2] {id = 3;next_id = 4;depth = 3} ex_ldv_ref

let deja_maj = !ex_ldv_ref  (* [{entier = [1; 2; 3]; node = {id = 2}}; {entier = [1; 2]; node = {id = 3}}] *)
(** test correcte! **)

let supprime_noeud id listeDejaVus_ref =
  let l = !listeDejaVus_ref in
  let rec aux l acc=
    match l with
      | [] -> acc
      | { entier = e ;node = n } :: tl -> 
        if n.id = id then aux tl acc
        else aux tl ({ entier = e ;node = n } :: acc)
  in aux l []




let maj_parent_noeud child_id next_correction l_ref =
  let l = !l_ref in
  let rec aux l acc =
    match l with
    | [] -> acc
    | { entier = e; node = n } :: tl ->
      if n.next_id = child_id then
        aux tl ({ entier = e; node = { id = n.id; next_id = next_correction; depth = n.depth } } :: acc)
      else
        aux tl ({ entier = e; node = n } :: acc)
  in let _ = l_ref := (aux l []) in ()
    


let compressionParListe arbre_decision =
  let listeDejaVus_ref = ref [] in
  let num = ref 1 in

  let rec aux arbre_decision =
    let ge = liste_feuille_to_ge (liste_feuilles arbre_decision) in
    match arbre_decision with
    | Leaf b -> ()
    | Node (depth, l, r) ->
      if check_ldv ge !listeDejaVus_ref then
        let _ = maj_parent_noeud (!num) (!num + 1) listeDejaVus_ref in
        let _ = listeDejaVus_ref := supprime_noeud (!num) listeDejaVus_ref in
        let _ = num := !num + 1 in
        let _ = aux l in
        aux r
      else
        let new_node = { id = !num; next_id = !num + 1; depth = depth } in
        let element = { entier = ge; node = new_node } in
        let _ = listeDejaVus_ref := element :: !listeDejaVus_ref in
        let _ = num := !num + 1 in
        let _ = aux l in
        aux r
  in
  let _ = aux arbre_decision in
  !listeDejaVus_ref


  let rec is_left_true node =
    match node with
    | Leaf b -> b
    | Node (_, _, right) -> is_left_true right

  let compressionParListe arbre_decision =
    let listeDejaVus = ref [] in
  
    let rec compress arbre =
      match arbre with
      | Leaf b -> arbre
      | Node (depth, left, right) ->
        let feuilles = liste_feuilles arbre_decision in
        let n = liste_feuille_to_ge feuilles in
  
        (* Vérifie si le grand entier n est déjà dans la listeDejaVus *)
        let find_in_list n =
          let find_element e = e.entier = n in
          List.find_opt find_element !listeDejaVus
        in
  
        match find_in_list n with
        | Some element ->
          (* Règle-M : Remplace l'arête pointant vers l'actuel nœud par l'arête pointant vers l'élément correspondant *)
          Node (depth, compress left, compress right)
        | None ->
          (* Ajoute le nœud actuel à la listeDejaVus *)
          let new_node = { id = depth; next_id = -1; depth = depth } in
          let new_element = { entier = n; node = new_node } in
          let _ = listeDejaVus := new_element :: !listeDejaVus in
  
          (* Vérifie la règle Z *)
          if is_left_true right then
            (* Règle-Z : Remplace l'arête pointant vers l'actuel nœud par l'arête pointant vers le fils gauche *)
            compress left
          else
            (* Conserve l'actuel nœud et continue la compression des sous-arbres *)
            Node (depth, compress left, compress right)
    in
  
    compress arbre_decision



(** test **)
let ex_25899 = decomposition 25899
let dbt_25899 = cons_arbre ex_25899
let test_compression_par_liste_25899 =  compressionParListe dbt_25899 


(** test not end and try to understand l'algo ZDD **)




(**********************Question 3.12 ********************)

(** Utilisation de ficher en ocaml **)
let file_test = "./projet_ocaml_2023/test.dot"

let () = 
  let message = "hello world\ntest\n" in
  let oc =open_out file_test in
  Printf.fprintf oc "%s\n" message;
  close_out oc


(** l'implémentation de sémantique de dot **)
(* let cons_graphe_dot arbre_de_decision = "TEST\n" *)
let cons_graphe_dot arbre_de_decision = failwith "todo"

(** Le principale de construction d'un ficher dot **)    
let file_graphe = "./projet_ocaml_2023/graphe.dot"
let dot fichier arbre_de_decision = 
  let _ = () in 
    let graphe = cons_graphe_dot arbre_de_decision in
    let oc = open_out file_graphe in
    Printf.fprintf oc "%s\n" graphe;
    close_out oc
    
let test_graphe = dot file_graphe dbt_25899
     




(**********************Question 3.13 ********************)
(* On a déjà fait cette question quand on teste la question 2.8 *)
let ex_25899 = decomposition 25899
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

(******  TODO: comprendre Q3.11 et Q3.12 ici c'est juste vérifier  *****)



(**********************Question 4.15 ********************)
type graphe_node = { id: int; (* Autres informations liées au nœud *) }

type arbre_deja_vus =
  | Noeud of graphe_node * arbre_deja_vus * arbre_deja_vus
  | Feuille








