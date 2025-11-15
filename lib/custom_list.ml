let rec length = function 
| [] -> 0
| _::tail -> 1 + length tail

let compare_lengths l1 l2 =
  compare (length l1) (length l2)

let rec compare_length_with lst len = 
  match lst with
  | [] -> compare 0 len 
  | _::tail ->
      if len <= 0 then 1
      else compare_length_with tail (len - 1) 

      
let is_empty = function
  | [] -> true
  | _ -> false

let cons x xs = x :: xs

let hd = function
  | [] -> failwith "hd: empty list"
  | head::__ -> head

let tl = function
  | [] -> failwith "tl: empty list"
  | __::tail -> tail

let rec nth lst n =
  match lst, n with
  | [], _ -> failwith "nth: index out of bounds"
  | x::_, 0 -> x
  | _::tail, n when n > 0 -> nth tail (n - 1)
  | _ -> raise (Invalid_argument "nth: negative index")

let rec nth_opt lst n =
  match lst, n with
  | [], _ -> None
  | x::_, 0 -> Some x
  | _::tail, n when n > 0 -> nth_opt tail (n - 1)
  | _ -> raise (Invalid_argument "nth: negative index")

let rev lst =
  let rec aux acc = function
    | [] -> acc
    | x::tail -> aux (x::acc) tail
  in
  aux [] lst  

  let init len f = 
    let rec aux i acc =
      if i < 0 then acc
      else aux (i - 1) ((f i):: acc)
    in aux (len - 1) []


  let append lst1 lst2 =
    let rec aux acc = function
      | [] -> acc
      | x::tail -> aux (x::acc) tail
    in
    let rev_lst1 = aux [] lst1 in
    let rec aux2 acc = function
      | [] -> acc
      | x::tail -> aux2 (x::acc) tail
    in
    aux2 lst2 rev_lst1