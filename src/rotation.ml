open Matrix
open Vect

type t = Vect.t

let make x y z = Vect.make x y z;;

let id = make 0.0 0.0 0.0;;

let matrix_rotation r =
  let mx =
    let cosx = cos (Vect.vx r) in
    let sinx = sin (Vect.vx r) in
    Matrix.make (1.,0.,0.) (0.,cosx,-.sinx) (0.,sinx,cosx)
  in 
  let my =
    let cosy = cos (Vect.vy r) in
    let siny = sin (Vect.vy r) in
    Matrix.make (1.,0.,0.) (0.,cosy,-.siny) (0.,siny,cosy)
  in 
  let mz =
    let cosz = cos (Vect.vz r) in
    let sinz = sin (Vect.vz r) in
    Matrix.make (1.,0.,0.) (0.,cosz,-.sinz) (0.,sinz,cosz)
  in 
  let product = Matrix.prod mz my in

  Matrix.prod product mx
;;

let compose = fun _ -> failwith "TODO";;

let apply r v =
  let m = matrix_rotation r in
  let vect_prod l =
    (l.c1 *. (vx v)) +. (l.c2 *. (vy v)) +. (l.c3 *. (vz v))
  in
  Vect.make (vect_prod m.l1) (vect_prod m.l2) (vect_prod m.l3)
;;

let print r =
  print_endline (Printf.sprintf ("[%f,%f,%f]") (Vect.vx r) (Vect.vy r) (Vect.vz r))
;;
