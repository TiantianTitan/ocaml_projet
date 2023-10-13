
(**********************Question 1.1*********************)


(**********************Question 1.2 ********************)

let decomposition num =
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

let rec pow a n =
  if n = 0 then 1
  else a* (pow a (n-1))

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




