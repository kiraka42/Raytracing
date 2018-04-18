type t = 
{
  distance_camera : float;
  l : int;
  h : int;
}

val make : int -> int -> Camera.t -> t
