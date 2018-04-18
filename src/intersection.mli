type ray =
{
  o : Vect.t;
  d : Vect.t;
}

exception NO_INTERSECTION

val create_ray : Vect.t -> Vect.t -> ray

val ray_point_at : ray -> float -> Vect.t

val inter_plane : ray -> Object.plane -> (float * Vect.t)

val inter_sphere : ray -> Object.sphere -> (float * Vect.t)

val inter_face : ray -> Object.face -> Vect.t -> Vect.t -> float -> float -> (float * Vect.t)

val inter_box : ray -> Object.box -> (float * Vect.t)

val inter_obj : ray -> Object.obj -> (float * Vect.t)

val inter_first : ray -> Object.t list -> (float * Vect.t * Object.texture * Object.t list)

val inter_obj_list : ray -> Object.t list -> (float * Vect.t * Object.texture)

val inter_bool : ray -> Object.t list -> bool

val color_view_by_ray : ray -> int -> Object.t list -> float -> Light.t list -> Color.t

