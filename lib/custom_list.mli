val length : 'a list -> int
val compare_lengths : 'a list -> 'a list -> int
val compare_length_with : 'a list -> int -> int
val is_empty : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val nth_opt : 'a list -> int -> 'a option
val rev : 'a list -> 'a list
val init : int -> (int -> 'a) -> 'a list
val append : 'a list -> 'a list -> 'a list