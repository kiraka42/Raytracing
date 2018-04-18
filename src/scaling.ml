type t = float

let points scal p =
  Vect.shift scal p
;;

let dimensions scal d =
  scal *. d
;;
