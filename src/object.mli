type texture =
{
  kd : float;
  ks : float;
  phong : float;
  color : Color.t;
}

type plane =
{
  vect_unit_normal : Vect.t;
  relative_dist : float;
}

type sphere =
{
  center : Vect.t;
  ray : float;
}

type face =
{
  plane : plane;
  opposite_plane : plane;
  center : Vect.t;
  opposite_center : Vect.t;
  dist_between_planes : float;
}

type box =
{
  side1 : face; (* Face devant *)
  side2 : face; (* Face droite *)
  side3 : face; (* Face haut   *)
}

type obj =
| Sphere of sphere
| Plane of plane
| Box of box 

type t = (obj * texture)  


