let x = 1 in x



let rec fuck a b = 
  match a with
  | [] -> b
  | hd :: tl -> hd + fuck tl b 