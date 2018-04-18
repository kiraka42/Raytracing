type t = Vect.t

let points t point = Vect.add t point;;

let relative_dist t d vect_unit_normal =
  d +. (Vect.scalprod t vect_unit_normal)
;;
