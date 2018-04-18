type line = 
{ 
  c1 : float; 
  c2 : float; 
  c3 : float; 
}
;;

type matrix = 
{ 
  l1 : line; 
  l2 : line; 
  l3 : line; 
}
;;

let make x y z =
  let get line = match line with | (a,b,c) -> { c1 = a; c2 = b; c3 = c; } in
{
  l1 = get x;
  l2 = get y;
  l3 = get z;
}
;;

let id = make (1.,0.,0.) (0.,1.,0.) (0.,1.,0.) 

let prod m1 m2 =
  let get_line line =
    {
      c1 = (line.c1 *. m2.l1.c1) +. (line.c2 *. m2.l2.c1) +. (line.c3 *. m2.l3.c1);
      c2 = (line.c1 *. m2.l1.c2) +. (line.c2 *. m2.l2.c2) +. (line.c3 *. m2.l3.c2);
      c3 = (line.c1 *. m2.l1.c3) +. (line.c2 *. m2.l2.c3) +. (line.c3 *. m2.l3.c3);
    }
  in
  {
    l1 = get_line m1.l1;
    l2 = get_line m1.l2;
    l3 = get_line m1.l3;
  }
;;

let print_line l = 
  print_endline (Printf.sprintf ("[%f,%f,%f]") l.c1 l.c2 l.c3)
;;

let print m =
  print_line m.l1;
  print_line m.l2;
  print_line m.l3;
;;
