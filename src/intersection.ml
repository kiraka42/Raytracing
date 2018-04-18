open Object 
open Light

exception NO_INTERSECTION;;

type ray = 
{
  o : Vect.t; 
  d : Vect.t; 
}
;;

let create_ray o d =
  {
    o = o;
    d = d;
  }
;;

let ray_point_at r dist = (Vect.add r.o (Vect.shift dist r.d));;

let inter_plane r plane = 
  (* N.OS <= d : coté intérieur *)
  let prod_scal_in = (Vect.scalprod plane.vect_unit_normal r.o) in
  if (prod_scal_in <= plane.relative_dist)
  then raise NO_INTERSECTION
  else
    (* N.D <= 0 : parallele au plan *)
    let prod_scal_parallel = (Vect.scalprod plane.vect_unit_normal r.d) in
    if (prod_scal_parallel = 0.)
    then raise NO_INTERSECTION
    else 
      let ti = (plane.relative_dist -. prod_scal_in) /. prod_scal_parallel in
      if (ti <= 0.)
      then raise NO_INTERSECTION
      else 
	(ti, plane.vect_unit_normal)
;; 

let inter_sphere r (sphere : Object.sphere) = 
  let sc_vec = (Vect.diff sphere.center r.o) in
  let sa_dist = (Vect.scalprod sc_vec r.d) in
  (* SC.D <= 0 *)
  if (sa_dist <= 0.) 
  then raise NO_INTERSECTION
  else
    let sc_dist2 = (Vect.dist2 r.o sphere.center) in
    let ac_dist2 = sc_dist2 -. (sa_dist *. sa_dist) in
    (* |AC| >= r *)
    if (ac_dist2 /. 2. >= sphere.ray)
    then raise NO_INTERSECTION
    else 
      let ab_dist = sqrt((sphere.ray *. sphere.ray) -. ac_dist2) in
      let sb_dist = sa_dist -. ab_dist in
      let b_point = (ray_point_at r sb_dist) in
      let cb_vec = (Vect.diff b_point sphere.center) in
      
      (sb_dist , (Vect.normalise cb_vec))
;;

let inter_face r face direction2 direction3 l2 l3 =
  let inter_plan_face plane center =
    let (ti,direction) = (inter_plane r plane) in
    let i_point = (ray_point_at r ti) in
    let ci_vect = Vect.diff center i_point in
    if abs_float (Vect.scalprod ci_vect direction2) <= l2 &&
      abs_float (Vect.scalprod ci_vect direction3) <= l3
    then (ti,direction)
    else raise NO_INTERSECTION
  in
  
  let prod_scal = (Vect.scalprod face.plane.vect_unit_normal r.d) in
  (* D.N1 = 0 *)
  if (prod_scal = 0.)
  then raise NO_INTERSECTION
  else 
    (* D.N1 > 0 : face opposée *)
    if (prod_scal > 0.)
    then 
      inter_plan_face face.opposite_plane face.opposite_center
    else
      (* D.N1 < 0 : face courrante *)
      inter_plan_face face.plane face.center
;;

let inter_box r box =
  try
    inter_face
      r box.side1 box.side2.plane.vect_unit_normal box.side3.plane.vect_unit_normal box.side2.dist_between_planes box.side3.dist_between_planes
  with NO_INTERSECTION ->
    try
      inter_face
	r box.side2 box.side1.plane.vect_unit_normal box.side3.plane.vect_unit_normal box.side1.dist_between_planes box.side3.dist_between_planes
    with NO_INTERSECTION ->
      inter_face
	r box.side3 box.side2.plane.vect_unit_normal box.side1.plane.vect_unit_normal box.side2.dist_between_planes box.side1.dist_between_planes
;;

let inter_obj r obj = match obj with
  | Plane plane -> inter_plane r plane
  | Sphere sphere -> inter_sphere r sphere
  | Box box -> inter_box r box
;; 

let rec inter_first r l = match l with
  | [] -> raise NO_INTERSECTION
  | (obj, texture)::t ->
    try 
      let (dist,direction) = inter_obj r obj in 
      (dist, direction, texture, t)
    with NO_INTERSECTION -> inter_first r t
;;

let inter_obj_list r l =
  let rec aux r dist direction texture list = match list with
    | [] -> (dist, direction, texture)
    | (obj, new_texture)::t ->
      try
	let (new_dist, new_direction) = inter_obj r obj in
	(* La nouvelle intersection est-elle plus proche de l'ancienne ? *)
	if new_dist < dist  
	then aux r new_dist new_direction new_texture t
	else aux r dist direction texture t
      with NO_INTERSECTION -> aux r dist direction texture t
  in
  let (d, v, text, t) = inter_first r l in
  aux r d v text t
;;

let inter_bool r l =
  try
    let _ = inter_first r l in true
  with NO_INTERSECTION -> false
;;

let color_view_by_ray ray recursive list_objects ambient lights = 

  let rec pow a = function
    | 0 -> 1.
    | 1 -> a
    | n -> 
      let b = pow a (n / 2) in
      b *. b *. (if n mod 2 = 0 then 1. else a)
  in

  let rec searh_color r recurs = 
      (* Si on a dépassé la borne max de nb d'appels récursifs, alorso n arrete *)
      if (recurs = 0)
      then raise NO_INTERSECTION
      else
	let (distance, direction, texture) = inter_obj_list r list_objects in      
	let impact_point = ray_point_at r distance in

	let rec get_visible_source_lights l = match l with
	  | [] -> []
	  | light::t ->
	    let light_ray = create_ray impact_point light.direction in
	    (* si intersection, la source de lumière n'est pas visible *)
	    if (inter_bool light_ray list_objects)
	    then get_visible_source_lights t
	    else light::(get_visible_source_lights t)
	in 

	let visible_lights = get_visible_source_lights lights in

	let rec sum_lights_source l = match l with
	  | [] -> 0.
	  | light::t ->
	    ((Vect.scalprod direction light.direction) *. light.intensity) 
	    +. (sum_lights_source t)
	in 
	
	let rec sum_lights_bissectant l = match l with
	  | [] -> 0.
	  | light::t -> 
	    let bissectant = Vect.normalised_diff light.direction r.d in
	    ((pow (Vect.scalprod direction bissectant) (int_of_float texture.phong)) 
	     *. light.intensity) +. (sum_lights_bissectant t)
	in 

	let prod_scal = Vect.scalprod r.d direction in
	let reflection_direction =
	  Vect.diff r.d (Vect.shift (2. *. prod_scal) direction) in
	let reflection_ray = create_ray impact_point reflection_direction in

	let reflection_color = 
	  try
	    searh_color reflection_ray (recurs - 1) 
	  with NO_INTERSECTION -> Color.black
	in

	let first = Color.shift (texture.kd *. ambient) texture.color in
	let second = Color.shift (texture.kd *. (sum_lights_source visible_lights)) texture.color in
	let third = Color.shift (texture.ks *. (sum_lights_bissectant visible_lights)) texture.color in
	let fourth = Color.shift texture.ks reflection_color in

	Color.add (Color.add (Color.add first second) third) fourth
  in

  try
    searh_color ray recursive
  with NO_INTERSECTION -> Color.black
;;

