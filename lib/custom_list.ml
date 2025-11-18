let rec length = function 
| [] -> 0
| _::tail -> 1 + length tail

let compare_lengths l1 l2 =
  Stdlib.compare (length l1) (length l2)

let rec compare_length_with lst len = 
  match lst with
  | [] -> Stdlib.compare 0 len 
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
  in aux [] lst  

let init len f = 
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) ((f i):: acc)
  in aux (len - 1) []

let rec append lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | head::tail -> head :: (append tail lst2)
  
let rev_append lst1 lst2 = append (rev lst1) lst2

let concat lst = 
  let rec aux acc = function 
    | [] -> acc
    | hd::tail -> append hd (aux acc tail)
  in aux [] lst

let flatten = concat

let rec equal cmp l1 l2 = match (l1,l2) with 
  | [],[] -> true
  | [],_ | _,[] -> false
  | hd1::tl1, hd2::tl2 when cmp hd1 hd2 -> equal cmp tl1 tl2
  | _,_ -> false

let rec compare cmp l1 l2 = match l1,l2 with 
  | [],[] -> 0
  | [],_ -> -1
  | _,[] -> 1
  | hd1::tl1, hd2::tl2 -> match cmp hd1 hd2 with  
    | 0 -> compare cmp tl1 tl2
    | x -> x 

let rec iter f = function 
  | [] -> ()
  | hd::tl -> (f hd) ; iter f tl

let iteri f lst = 
  let rec aux i = function 
    | [] -> ()
    | hd::tl -> f i hd ; aux (i+1) tl 
  in aux 0 lst 

let rec map f = function 
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let mapi f lst = 
  let rec aux i = function 
    | [] -> []
    | hd::tl -> (f i hd)::(aux (i+1) tl)
  in aux 0 lst 

let rev_map f lst =
  let rec aux acc = function
    | [] -> acc
    | x::tail -> aux ((f x)::acc) tail
  in aux [] lst  

let rec filter_map f = function 
  | [] -> []
  | hd::tl -> match f hd with
    | None -> filter_map f tl 
    | Some a -> a::(filter_map f tl) 

let concat_map f lst = 
  let rec aux acc = function 
    | [] -> acc
    | hd::tail -> append (f hd) (aux acc tail)
  in aux [] lst

let rec fold_left f acc = function 
  | [] -> acc
  | hd::tl -> fold_left f (f acc hd) tl


let rec fold_right f lst acc = match lst with
  | [] -> acc
  | hd::tl -> f hd (fold_right f tl acc)

let rec iter2 f lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> ()
  | hd1::tl1, hd2::tl2 -> f hd1 hd2 ; iter2 f tl1 tl2
  | _ -> raise (Invalid_argument "iter2: lists have different lengths")

let rec map2 f lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> []
  | hd1::tl1, hd2::tl2 -> (f hd1 hd2)::(map2 f tl1 tl2)
  | _ -> raise (Invalid_argument "map2: lists have different lengths")

let rev_map2 f lst1 lst2 =
  let rec aux acc l1 l2 = match l1,l2 with
    | [],[] -> acc
    | hd1::tl1,hd2::tl2 -> aux ((f hd1 hd2)::acc) tl1 tl2
    | _ -> raise (Invalid_argument "rev_map2: lists have different lengths")
  in aux [] lst1 lst2

let rec fold_left2 f acc lst1 lst2 =
  match lst1, lst2 with
    | [], [] -> acc
    | hd1::tl1, hd2::tl2 -> 
        let acc2 = f acc hd1 hd2 in
        fold_left2 f acc2 tl1 tl2
    | _ -> raise (Invalid_argument "fold_left2: lists have different lengths")

let rec fold_right2 f lst1 lst2 acc = match lst1,lst2 with
  | [],[] -> acc
  | hd1::tl1,hd2::tl2 -> f hd1 hd2 (fold_right2 f tl1 tl2 acc)
  | _ -> raise (Invalid_argument "fold_right2: lists have different lengths")

let rec for_all p = function
  | [] -> true
  | hd::tl -> p hd && for_all p tl

let rec exists p = function
  | [] -> false
  | hd::tl -> p hd || exists p tl

let rec for_all2 p lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> true
  | hd1::tl1, hd2::tl2 -> p hd1 hd2 && for_all2 p tl1 tl2
  | _ -> raise (Invalid_argument "for_all2: lists have different lengths")

let rec exists2 p lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> false
  | hd1::tl1, hd2::tl2 -> p hd1 hd2 || exists2 p tl1 tl2
  | _ -> raise (Invalid_argument "exists2: lists have different lengths") 

let rec mem x = function
  | [] -> false
  | hd::tl -> hd = x || mem x tl

let rec memq x = function
  | [] -> false
  | hd::tl -> hd == x || memq x tl

let rec find p = function
  | [] -> raise Not_found
  | hd::_ when p hd -> hd
  | _::tl -> find p tl

let rec find_opt p = function
  | [] -> None
  | hd::_ when p hd -> Some hd
  | _::tl -> find_opt p tl

let find_index p lst = 
  let rec aux i = function
    | [] -> None
    | hd::_ when p hd -> Some i
    | _::tl -> aux (i+1) tl
  in aux 0 lst

let rec find_map f lst =
  match lst with
  | [] -> None
  | hd::tl -> match f hd with
    | None -> find_map f tl
    | Some a -> Some a

let find_mapi predicate lst =
  let rec aux i = function
    | [] -> None
    | hd::tl -> match predicate i hd with
      | None -> aux (i+1) tl
      | Some a -> Some a
  in aux 0 lst

let rec filter p = function
  | [] -> []
  | hd::tl -> 
      if p hd 
      then hd :: filter p tl
      else filter p tl

let find_all = filter

let filteri p lst =
  let rec aux i = function
    | [] -> []
    | hd::tl ->
        if p i hd 
        then hd :: aux (i+1) tl
        else aux (i+1) tl
  in aux 0 lst

let rec take n = function 
  | _ when n < 0 -> raise (Invalid_argument "take: negative count")
  | _ when n = 0 -> []
  | [] -> []
  | hd::tl -> hd :: take (n - 1) tl

let rec drop n = function 
  | _ when n < 0 -> raise (Invalid_argument "drop: negative count")
  | lst when n = 0 -> lst
  | [] -> []
  | _::tl -> drop (n - 1) tl

let rec take_while p = function
  | [] -> []
  | hd::tl when p hd -> hd :: take_while p tl
  | _ -> []

let rec drop_while p = function
  | [] -> []
  | hd::tl when p hd -> drop_while p tl
  | lst -> lst

let rec partition p = function
  | [] -> [],[]
  | hd::tl -> let yes,no = partition p tl in
      if p hd 
      then hd::yes, no
      else yes, hd::no

let rec partition_map f = function
  | [] -> [],[]
  | hd::tl -> 
      let yes,no = partition_map f tl in
      match f hd with
      | Either.Left a -> (a::yes, no)
      | Either.Right b -> (yes, b::no)

let rec assoc key = function
  | [] -> raise Not_found
  | (k,v)::_ when k = key -> v 
  | _::tl -> assoc key tl

let rec assoc_opt key = function
  | [] -> None
  | (k,v)::_ when k = key -> Some v
  | _::tl -> assoc_opt key tl


let rec assq key = function
  | [] -> raise Not_found
  | (k,v)::_ when k == key -> v 
  | _::tl -> assq key tl

let rec assq_opt key = function
  | [] -> None
  | (k,v)::_ when k == key -> Some v
  | _::tl -> assq_opt key tl

let rec mem_assoc key = function
  | [] -> false
  | (k,_)::_ when k = key -> true 
  | _::tl -> mem_assoc key tl

let rec mem_assq key = function
  | [] -> false
  | (k,_)::_ when k == key -> true 
  | _::tl -> mem_assq key tl

let rec remove_assoc key = function
  | [] -> []
  | (k,_)::tl when k = key -> tl
  | hd::tl -> hd :: remove_assoc key tl

let rec remove_assq key = function
  | [] -> []
  | (k,_)::tl when k == key -> tl
  | hd::tl -> hd :: remove_assq key tl

let rec split = function
  | [] -> ([],[])
  | (x,y)::tl -> 
      let xs,ys = split tl in
      (x::xs, y::ys)  

let rec combine lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> []
  | hd1::tl1, hd2::tl2 -> (hd1, hd2) :: combine tl1 tl2
  | _ -> raise (Invalid_argument "combine: lists have different lengths") 