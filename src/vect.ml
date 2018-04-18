type t = 
{
  vx : float; (* vx *)
  vy : float; (* vy *)
  vz : float; (* vz *)
}
;;

let vx v = v.vx;;
let vy v = v.vy;;
let vz v = v.vz;;

let make x y z =
{
  vx = x;
  vy = y;
  vz = z;
}
;;

let xunit = make 1. 0. 0.;;
let yunit = make 0. 1. 0.;;
let zunit = make 0. 0. 1.;;

let scalprod v1 v2 =
  (v1.vx *. v2.vx) +. (v1.vy *. v2.vy) +. (v1.vz *. v2.vz)
;;

let norm v =
  sqrt ((v.vx *. v.vx) +. (v.vy *. v.vy) +. (v.vz *. v.vz))
;;

(* méthode privée ajoutée pour généraliser les opérations sur vecteurs *)
let operation op v1 v2 =
{
  vx = op v1.vx v2.vx;
  vy = op v1.vy v2.vy;
  vz = op v1.vz v2.vz;
}
;;

let add v1 v2 = operation (+.) v1 v2;;

let diff v1 v2 = operation (-.) v1 v2;;

let shift s v =
{
  vx = s *. v.vx;
  vy = s *. v.vy;
  vz = s *. v.vz;
}
;;

let opp v = shift (-1.) v;;

let dist v1 v2 = norm (diff v1 v2);;

let normalise v = shift (1. /. (norm v)) v;;

let dist2 v1 v2 = (scalprod (diff v1 v2) (diff v1 v2)) ;;

let normalised_diff v1 v2 = normalise (diff v1 v2);;

let print v =
  print_endline (Printf.sprintf ("[%f,%f,%f]") v.vx v.vy v.vz)
;;
