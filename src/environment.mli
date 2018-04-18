type key = string
type 'a t

val empty : 'a t

val insert : 'a t -> key -> 'a -> 'a t

val find : 'a t -> key -> 'a

val fusion : key list -> 'a list -> 'a t

val add : 'a t ref -> key -> 'a -> unit

val to_list : 'a t -> 'a list
