
let decomposition num =
  if num = 0 then [false]
  else
  let rec aux num acc =
    if num <= 0 then acc
    else let modulo = num mod 2 in 
      if modulo = 1 then aux (num/2) (true :: acc)
      else aux (num/2) (false :: acc)
    in List.rev (aux num [])

let completion couple =
  let l,t = couple in
  let rec aux l t acc =
    if t = 0 then acc
    else match l with
      | [] -> aux l (t-1) (false::acc)
      | hd :: tl -> aux tl (t-1) (hd::acc)
  in List.rev (aux l t []) 

let composition lb = 
  let rec aux lb acc =
    match lb with
    | [] -> acc
    | hd :: tl -> if hd = true then aux tl (acc*2+1)
                  else aux tl (acc*2)
  in aux (List.rev lb) 0


type decision_binary_tree =
  | Leaf of bool
  | Node of int * decision_binary_tree * decision_binary_tree

let rec log2_arete n =
  if n <= 0 then failwith "n <= 0"
  else if n = 1 || n = 2 then 1
  else 1 + log2_arete ((n+1)/2)

let arete_arbre lb = log2_arete (List.length lb)
let cons_chemin_list lb =
  let length = List.length lb in
  let rec aux lb cpt acc =
    match lb with
    | [] -> acc
    | hd :: tl -> let com = List.rev (completion ( (decomposition cpt), log2_arete length) ) in aux tl (cpt+1) (  (com ,hd ) :: acc)
    in  List.rev (aux lb 0 []) 

let cons_vide_arbre n =
  if n = 0 then failwith "Empty tree"
  else 
    let rec constructor arete =
      if arete = n+1 then Leaf(false)
      else Node (arete, constructor (arete+1),constructor (arete+1))
    in constructor 1


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


let cons_arbre lb = 
  let chemin_bool_list = cons_chemin_list lb in
  let arete = arete_arbre lb in
  let arbre_init = cons_vide_arbre arete in
  let rec inserer_list chemin_bool_list acc =
    match chemin_bool_list with
    | [] -> acc
    | hd :: tl -> inserer_list tl (inserer hd acc)
    in inserer_list chemin_bool_list arbre_init 

let liste_feuilles arbre =
  let rec aux arbre acc =
    match arbre with
      | Leaf (b) -> b :: acc
      | Node (_,l,r) -> aux r (aux l acc)
  in List.rev (aux arbre [])

type graphe_node = {id:int list; depth: int }

type elements = { 
    entier: int list; 
    node_l: graphe_node ref;
    node_r: graphe_node ref
}

type liste_deja_vus = elements list

let rec sup_prefix0 l =
  match l with
    | [] -> []
    | hd :: tl -> 
      if hd = 0 then sup_prefix0 tl
      else l 
 
let liste_feuille_to_ge lf =
  let rec aux lf acc =  
    let list_length = List.length lf in   
    match list_length <= 64 with
    | true -> List.rev ( sup_prefix0 ((composition lf) ::acc )) 
    | false -> aux (List.rev (completion ((List.rev lf),(list_length - 64)))) ((composition (completion (lf,64))) ::acc)
  in aux lf []

let decomposition_ge ge_num_liste =
  let rec aux ge_num_liste acc =
  match ge_num_liste with
    | [] -> acc
    | [x] -> acc @ (decomposition x)
    | hd :: tl ->  aux tl ( acc @ ( completion ((decomposition hd),64))) 
  in aux ge_num_liste []


let rec check_ldv n ldv =
  match ldv with
  | [] -> false
  | hd :: tl ->  
  if  n = hd.entier then true
  else if n = !(hd.node_l).id then true
  else if n = !(hd.node_r).id then true
  else check_ldv n tl





let maj_noeud_l node_ref ge liste_ref =
  let liste = !liste_ref in
  let rec aux l acc =
  match l with
  | [] -> acc
  | { entier = e; node_l = n_l; node_r = n_r } :: tl -> 
  if e = ge then aux tl ({entier = e; node_l = node_ref;node_r= n_r}:: acc) 
  else aux tl ({entier = e; node_l = n_l; node_r = n_r} :: acc)
  in aux liste []

  let maj_noeud_r node_ref ge liste_ref =
    let liste = !liste_ref in
    let rec aux l acc =
    match l with
    | [] -> acc
    | { entier = e; node_l = n_l; node_r = n_r } :: tl -> 
    if e = ge then aux tl ({entier = e; node_l = n_l;node_r= node_ref}:: acc) 
    else aux tl ({entier = e; node_l = n_l; node_r = n_r} :: acc)
    in aux liste []

  let rec get_ref_noeud ge liste_ref =
    let liste = !liste_ref in
    match liste with
    | [] -> failwith "ge n'est pas dans la liste"
    | { entier = e; node_l = n_l; node_r = n_r } :: tl -> 
    if e = [-1] then   ref {id = [-1]; depth = -1}
    else if e = [-2] then   ref {id = [-2]; depth = -1}
    else if (!n_l).id = ge then n_l
    else if  (!n_r).id = ge then n_r
    else get_ref_noeud ge (ref tl)


  let compressionParListe arbre_decision =
    let listeDejaVus_ref = ref [] in
    let new_element_t = {entier = [-1]; node_l = ref {id = [];depth = -1}; node_r = ref {id = [];depth = -1}} in
    let new_element_f = {entier = [-2]; node_l = ref {id = [];depth = -1}; node_r = ref {id = [];depth = -1}} in
    let _ = listeDejaVus_ref := new_element_t :: new_element_f :: !listeDejaVus_ref in
       
    let rec aux ad =
      let ge = liste_feuille_to_ge (liste_feuilles ad) in
      match ad with
      | Leaf b -> failwith "Wont reach here"
      | Node (d,Leaf(true),Leaf(false)) ->
        if check_ldv ge !listeDejaVus_ref then
          let _ = maj_noeud_l (get_ref_noeud [-1] listeDejaVus_ref) ge listeDejaVus_ref in
          let _ = maj_noeud_r (get_ref_noeud [-1] listeDejaVus_ref) ge listeDejaVus_ref in ()
        else  let new_node_l = {id = [-1] ;depth = d} in let new_node_r = {id = [-1] ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l;node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in ()
      | Node (d,Leaf(false),Leaf(false)) ->
        if check_ldv ge !listeDejaVus_ref then
          let _ = maj_noeud_l (get_ref_noeud [-2] listeDejaVus_ref) ge listeDejaVus_ref in
          let _ = maj_noeud_r (get_ref_noeud [-2] listeDejaVus_ref) ge listeDejaVus_ref in()
        else  let new_node_l = {id = [-2] ;depth = d} in let new_node_r = {id = [-2] ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l;node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in ()
      | Node (d,Leaf(false),Leaf(true)) ->
        if check_ldv ge !listeDejaVus_ref then
          let _ = maj_noeud_l (get_ref_noeud [-2] listeDejaVus_ref) ge listeDejaVus_ref in
          let _ = maj_noeud_r (get_ref_noeud [-1] listeDejaVus_ref) ge listeDejaVus_ref in ()
        else  let new_node_l = {id = [-2] ;depth = d} in let new_node_r = {id = [-1] ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l;node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in ()
      | Node (d,Leaf(true),Leaf(true)) ->
        if check_ldv ge !listeDejaVus_ref then
          let _ = maj_noeud_l (get_ref_noeud [-1] listeDejaVus_ref) ge listeDejaVus_ref in
          let _ = maj_noeud_r (get_ref_noeud [-1] listeDejaVus_ref) ge listeDejaVus_ref in ()
        else  let new_node_l = {id = [-1] ;depth = d} in let new_node_r = {id = [-1] ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l;node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in ()
      | Node (d, l, r) -> 
        let ge_l = liste_feuille_to_ge (liste_feuilles l) in 
        let ge_r = liste_feuille_to_ge (liste_feuilles r) in
        let _ =
        if not (check_ldv ge_l !listeDejaVus_ref) && not (check_ldv ge_r !listeDejaVus_ref) then 
          let new_node_l = {id = ge_l ;depth = d} in
          let new_node_r = {id = ge_r ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l;node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in ()
        else if check_ldv ge_l !listeDejaVus_ref && check_ldv ge_r !listeDejaVus_ref then
          let _ = maj_noeud_l (get_ref_noeud ge_l listeDejaVus_ref) ge listeDejaVus_ref in 
          let _ = maj_noeud_r   (get_ref_noeud ge_r listeDejaVus_ref) ge listeDejaVus_ref in 
          let _ = aux l in aux r
        else if check_ldv ge_l !listeDejaVus_ref then 
          let new_node_r = {id = ge_r ;depth = d} in
          let new_element = {entier = ge; node_l = ref {id = [-1];depth = d+1};node_r = ref new_node_r} in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in 
          let _ = maj_noeud_l (get_ref_noeud ge_l listeDejaVus_ref) ge listeDejaVus_ref in ()
        else if  check_ldv ge_r !listeDejaVus_ref then
          let new_node_l = {id = ge_r ;depth = d} in
          let new_element = {entier = ge; node_l = ref new_node_l ;node_r =ref {id = [-1];depth = d+1} } in
          let _ = listeDejaVus_ref := new_element :: !listeDejaVus_ref in 
          let _ = maj_noeud_r   (get_ref_noeud ge_r listeDejaVus_ref) ge listeDejaVus_ref in let _ = aux l in aux r
        
        in
        let _ = aux l in aux r
    in  let _ = (aux arbre_decision) in !listeDejaVus_ref


    let ex_25899 = decomposition 25899
    let dbt_25899 = cons_arbre ex_25899

    let arbre_compress = compressionParListe dbt_25899 






