
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

type node_id_type = GE of int list | BL of bool

type graphe_node = {id:node_id_type }

type elements = { 
    entier: node_id_type; 
    node_l: graphe_node ref;
    node_r: graphe_node ref;
    depth: int
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
   (*** explication:  si length > 64, acc @ les premiers 64 ensuite on la reste sauf que les premiers 64 va continuer la récursive ***)
    in let ret = aux lf [] in
  if ret = [] then [0]
  else ret

let decomposition_ge ge_num_liste =
  let rec aux ge_num_liste acc =
  match ge_num_liste with
    | [] -> acc
    | [x] -> acc @ (decomposition x)
    | hd :: tl ->  aux tl ( acc @ ( completion ((decomposition hd),64))) 
  in aux ge_num_liste []


(* let rec check_ldv n ldv =
  match ldv with
  | [] -> false
  | hd :: tl ->  
  if  n = hd.entier then true
  else if n = !(hd.node_l).id then true
  else if n = !(hd.node_r).id then true
  else check_ldv n tl *)


  let rec check_ldv n ldv =
    match ldv with
    | [] -> false
    | hd :: tl ->  if  n = hd.entier then true
    else check_ldv n tl


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
  if e = e_sup  || e = GE([0]) then aux tl acc
  else if !l.id = e_sup then aux tl ({entier = e;node_l = replace;node_r = r;depth=d} :: acc)
  else if !r.id = e_sup || !r.id = GE([0]) then aux tl ({entier = e;node_l = l;node_r = replace;depth=d} :: acc)
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
let a = ref [{entier = GE [0]; node_l = {contents = {id = BL false}};
  node_r = {contents = {id = BL false}}; depth = 4};
  {entier = GE [3]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL true}}; depth = 4}
  ;{entier = GE [0]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL false}}; depth = 4};
  {entier = GE [2]; node_l = {contents = {id = GE [2]}};
  node_r = {contents = {id = GE [0]}}; depth = 3};
  {entier = GE [0]; node_l = {contents = {id = BL false}};
    node_r = {contents = {id = BL false}}; depth = 4};
    {entier = GE [2]; node_l = {contents = {id = GE [2]}};
node_r = {contents = {id = GE [0]}}; depth = 3};
{entier = BL false; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL false}}; depth = -1}
]
let f = maj_regle_Z a
let b = !a



let m_change ldv_ref e_change replace=
  let rec aux ldv acc =
    match ldv with
   | [] -> acc
   |({entier = e; node_l = l; node_r = r; depth = d} as hd) :: tl -> 
    if e_change = e then aux tl acc
    else if !l.id = e_change then aux tl ({entier = e;node_l = replace;node_r = r;depth=d} :: acc)
    else if !r.id = e_change then aux tl ({entier = e;node_l = l;node_r = replace;depth=d} :: acc)
    else aux  tl (hd::acc)
    in let ret = aux !ldv_ref []  in ldv_ref := ret
(*
let rec maj_regle_M ldv_ref =
  match !ldv_ref with
  | [] -> ()
  | hd :: tl -> 
    let rec aux_tl tl  =
      match tl with
      | [] -> ()
      | hd2 :: tl2 -> 
      if hd.entier = hd2.entier then let _ = z_sup ldv_ref hd2.entier (ref{id = hd.entier}) in aux_tl tl2
      else aux_tl tl2
     in let _ = aux_tl tl in maj_regle_M (ref tl) *)




let m_connect replace ldv_ref = 
  let rec aux ldv acc =
    match ldv with
   | [] -> acc
   |({entier = e; node_l = l; node_r = r; depth = d} as hd) :: tl -> 
   if !l.id = replace then aux tl ({entier = e;node_l = ref {id = replace} ;node_r = r;depth=d} :: acc)
   else if !l.id = replace then aux tl ({entier = e;node_l = l;node_r = ref {id = replace};depth=d} :: acc)
   else aux ldv (hd::acc)
  in let ret = aux !ldv_ref [] in ldv_ref := ret



let  maj_regle_M ldv_ref =
  let ret = ref [] in
  let ldv = !ldv_ref in 
  let rec aux ldv =
  match ldv with
  | [] -> ()
  | hd :: tl -> 
    if check_ldv hd.entier !ret then let _ =  m_connect hd.entier ldv_ref in aux tl
    else let _ = ret := hd :: !ret in aux tl
  in let _ = aux ldv in ldv_ref := !ret




let a = ref [
  {entier = GE [2]; node_l = {contents = {id = BL false}};
  node_r = {contents = {id = BL true}}; depth = 4};
 {entier = GE [2]; node_l = {contents = {id = GE [2]}};
  node_r = {contents = {id = GE [1]}}; depth = 3};
 {entier = GE [2]; node_l = {contents = {id = BL false}};
  node_r = {contents = {id = BL true}}; depth = 4};
 {entier = GE [3]; node_l = {contents = {id = BL true}};
  node_r = {contents = {id = BL true}}; depth = 4};

]
let ex_25899 = decomposition 25899
let dbt_25899 = cons_arbre ex_25899

let a = ref (dbt_to_liste_deja_vus dbt_25899)
let a_ = ref [{entier = GE [2]; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL true}}; depth = 4};
{entier = GE [6]; node_l = {contents = {id = GE [2]}};
node_r = {contents = {id = GE [1]}}; depth = 3};
{entier = GE [5]; node_l = {contents = {id = GE [1]}};
node_r = {contents = {id = GE [1]}}; depth = 3};
{entier = GE [101]; node_l = {contents = {id = GE [5]}};
node_r = {contents = {id = GE [6]}}; depth = 2};
{entier = GE [2]; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL true}}; depth = 4};
{entier = GE [2]; node_l = {contents = {id = GE [2]}};
node_r = {contents = {id = GE [1]}}; depth = 3};
{entier = GE [2]; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL true}}; depth = 4};
{entier = GE [3]; node_l = {contents = {id = BL true}};
node_r = {contents = {id = BL true}}; depth = 4};
{entier = GE [11]; node_l = {contents = {id = GE [3]}};
node_r = {contents = {id = GE [2]}}; depth = 3};
{entier = GE [43]; node_l = {contents = {id = GE [11]}};
node_r = {contents = {id = GE [2]}}; depth = 2};
{entier = GE [25899]; node_l = {contents = {id = GE [43]}};
node_r = {contents = {id = GE [101]}}; depth = 1};
{entier = BL true; node_l = {contents = {id = BL true}};
node_r = {contents = {id = BL true}}; depth = -1};
{entier = BL false; node_l = {contents = {id = BL false}};
node_r = {contents = {id = BL false}}; depth = -1}]
let b = maj_regle_M  a
let c = !a



let ldv_compare = dbt_to_liste_deja_vus dbt_25899
let ldv_show_Z = 
  let result = ref (dbt_to_liste_deja_vus dbt_25899) in
  let _ = maj_regle_Z result
  in ! result

let ldv_show_M =
  let result =  ref (dbt_to_liste_deja_vus dbt_25899) in
  let _ = maj_regle_M result 
  in ! result

