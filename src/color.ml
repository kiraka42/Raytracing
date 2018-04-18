type t = 
{
  red : float;   (* Rouge *)
  green : float; (* Vert  *)
  blue : float;  (* Bleu  *)
}
;;

let make r g b = 
  {
    red = r;
    green = g;
    blue = b;
  }
;;

let black = make 0. 0. 0.;;

let to_bytes c = 
(
  int_of_float(c.red *. 255.),
  int_of_float(c.green *. 255.),
  int_of_float(c.blue *. 255.)
)
;;

let to_graphics c = 
  let sup f =
    if (f > 255) then 255
    else if (f < 0) then 0
    else f
  in
  match to_bytes c with | (r,g,b) 
    -> Graphics.rgb (sup r) (sup g) (sup b)
;;

let add c1 c2 = 
  {
    red = c1.red +. c2.red;   
    green = c1.green +. c2.green;   
    blue = c1.blue +. c2.blue;   
  }
;;

let mult c1 c2 =
{
  red = c1.red *. c2.red;   
  green = c1.green *. c2.green;   
  blue = c1.blue *. c2.blue;   
}
;;

let shift s c =
{
  red = s *. c.red;   
  green = s *. c.green;   
  blue = s *. c.blue;   
}
;;

let print c = 
   print_endline (Printf.sprintf ("[%f,%f,%f]") c.red c.green c.blue)
;;

