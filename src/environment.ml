type key = string
type 'a t = (key * 'a) list

let empty : 'a t = [];;

let insert (e : 'a t) (k : key) (v : 'a) : 'a t = (k, v) :: e;;

let rec find (e : 'a t) (k : key) : 'a = match e with
  | [] -> failwith ("Unknown procedure environment key "^k)
  | (k',v)::t -> 
    if k = k'
    then v
    else find t k
;;

let fusion (keys : key list) (values : 'a list) : 'a t = 
  let rec aux e l1 l2 = match l1, l2 with
    | [],[] -> e
    | k::t1, v::t2 -> aux (insert e k v) t1 t2
    | _,_ -> failwith "Error"
  in

  aux empty keys values
;;

let add (e : 'a t ref) (k : key) (v : 'a) = 
  let rec aux e2 = match e2 with
    | [] -> insert e2 k v
    | (k',v')::t ->
      if k = k'
      then insert t k v
      else insert (aux t) k' v'
  in
  e := aux !e
;;

let rec to_list (e : 'a t) : 'a list = match e with
  | [] -> []
  | (k,v)::q -> v::(to_list q)
;;
