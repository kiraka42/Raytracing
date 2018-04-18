open Camera

type t = 
{
  distance_camera : float;
  l : int;
  h : int;
}
;;

let make width height camera =
  {
    distance_camera = float_of_int (width) /. (2. *. tan(camera.angle /. 2.));
    l = width;
    h = height;
  }
;;
