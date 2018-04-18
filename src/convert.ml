open Scenario
open Object
open Camera
open Light

let rec convert_expr expr env = match expr with
  | Bin (b, e1, e2) -> 
    let f = begin match b with
      | Plus -> (+.)
      | Minus -> (-.)
      | Mult -> ( *. )
      | Div -> (/.)
    end
    in
    f (convert_expr e1 env) (convert_expr e2 env)
  | Uni (u, e) -> 
    let f = begin match u with
      | Sin -> sin
      | Cos -> cos
      | Sqrt -> sqrt
      | Opp -> (~-.)
    end
    in
    f (convert_expr e env)
  | Const f -> f
  | Ident id -> Environment.find env id
;;

let rec convert_expr_list l env = match l with
  | [] -> []
  | h::t -> (convert_expr h env)::(convert_expr_list t env)
;;

let convert_color color env = match color with
    | (r,g,b) -> 
      Color.make 
	((convert_expr r env) /. 255.)
	((convert_expr g env) /. 255.)
	((convert_expr b env) /. 255.)
;;

let convert_rotation rotation env = match rotation with 
    | (r1,r2,r3) -> 
      Rotation.make (convert_expr r1 env) (convert_expr r2 env) (convert_expr r3 env)
;;

let convert_vector vector env = match vector with 
    | (x,y,z) -> 
      Vect.make (convert_expr x env) (convert_expr y env) (convert_expr z env)
;;

let convert_position position env = convert_vector position env;;

let convert_texture (texture:Scenario.texture) env : Object.texture  =
  {
    kd = convert_expr texture.kd env;
    ks = convert_expr texture.ks env;
    phong = convert_expr texture.phong env;
    color = convert_color texture.color env;
  }
;;

let convert_plane_translate plane t =
  {
    vect_unit_normal = plane.vect_unit_normal;
    relative_dist = 
      Translation.relative_dist t plane.relative_dist plane.vect_unit_normal
  }
;;

let convert_face_translate face t =
  {
    plane = convert_plane_translate face.plane t;
    opposite_plane = convert_plane_translate face.opposite_plane t;
    center = Translation.points t face.center;
    opposite_center = Translation.points t face.opposite_center;
    dist_between_planes = face.dist_between_planes;
  }
;;

let convert_obj_translate obj t = match obj with
  | Sphere sphere ->
    Sphere 
      {
	center = Translation.points t sphere.center;
	ray = sphere.ray;
      }
  | Plane plane -> Plane (convert_plane_translate plane t)
  | Box box ->
    Box
    {
      side1 = convert_face_translate box.side1 t;
      side2 = convert_face_translate box.side2 t;
      side3 = convert_face_translate box.side3 t;
    }
;;

let convert_plane_scale plane scal =
  {
    vect_unit_normal = plane.vect_unit_normal;
    relative_dist = 
      Scaling.dimensions scal plane.relative_dist
  }
;;

let convert_face_scale face scal =
  {
    plane = convert_plane_scale face.plane scal;
    opposite_plane = convert_plane_scale face.opposite_plane scal;
    center = Scaling.points scal face.center;
    opposite_center = Scaling.points scal face.opposite_center;
    dist_between_planes = Scaling.dimensions scal face.dist_between_planes;
  }
;;

let convert_obj_scale obj scal = match obj with
  | Sphere sphere ->
    Sphere 
      {
	center = Scaling.points scal sphere.center;
	ray = Scaling.dimensions scal sphere.ray;
      }
  | Plane plane -> Plane (convert_plane_scale plane scal)
  | Box box ->
    Box
      {
	side1 = convert_face_scale box.side1 scal;
	side2 = convert_face_scale box.side2 scal;
	side3 = convert_face_scale box.side3 scal;
      }
;;

let convert_plane_rotate plane r =
  {
    vect_unit_normal = Rotation.apply r plane.vect_unit_normal;
    relative_dist = plane.relative_dist
  }
;;

let convert_face_rotate face r =
  {
    plane = convert_plane_rotate face.plane r;
    opposite_plane = convert_plane_rotate face.opposite_plane r;
    center = face.center;
    opposite_center = face.opposite_center;
    dist_between_planes = face.dist_between_planes;
  }
;;

let convert_obj_rotate obj r = match obj with
  | Sphere sphere -> obj (* Pas de changement *)
  | Plane plane -> Plane (convert_plane_rotate plane r)
  | Box box ->
    Box
      {
	side1 = convert_face_rotate box.side1 r;
	side2 = convert_face_rotate box.side2 r;
	side3 = convert_face_rotate box.side3 r;
      }
;;

let rec convert_obj obj env_num env_obj = match obj with
  | Object id -> Environment.find env_obj id
  | Sphere (p, e, t) -> 
    (Sphere 
      {
	center = convert_position p env_num;
	ray = convert_expr e env_num;
      },
     convert_texture t env_num)
  | Plane (r, e, t) ->
    (Plane
      {
	vect_unit_normal = Rotation.apply (convert_rotation r env_num) Vect.yunit;
	relative_dist = convert_expr e env_num;
      },
     convert_texture t env_num)
  | Box (p, v, t) ->
    (* Le sommet commun des 3 faces est celui le plus en bas à à gauche derrière,
       on va aussi considérer que ce sommet est l'origine du repère *)
    let position = convert_vector p env_num in
    let dim = convert_vector v env_num in
    (Box
      {
	(* Face derriere *)
	side1 =
	  {
	    plane =
	      {
		vect_unit_normal = Vect.opp Vect.zunit;
		relative_dist = 0.;
	      };
	    opposite_plane =
	      {
		vect_unit_normal = Vect.opp Vect.zunit;
		relative_dist = Vect.vz dim;
	      };
	    center = Vect.add position (Vect.make ((Vect.vx dim) /. 2.) ((Vect.vy dim) /. 2.) 0.);
	    opposite_center =  Vect.add position
	      (Vect.make ((Vect.vx dim) /. 2.) ((Vect.vy dim) /. 2.) (Vect.vz dim));
	    dist_between_planes = (Vect.vz dim) /. 2.;
	  };
	(* Face gauche *)
	side2 =
	  {
	    plane =
	      {
		vect_unit_normal = Vect.opp Vect.xunit;
		relative_dist = 0.;
	      };
	    opposite_plane =
	      {
		vect_unit_normal = Vect.opp Vect.xunit;
		relative_dist = -. Vect.vx dim;
	      };
	    center = Vect.add position (Vect.make 0. ((Vect.vy dim) /. 2.) ((Vect.vz dim) /. 2.));
	    opposite_center = Vect.add position
	      (Vect.make (Vect.vx dim) ((Vect.vy dim) /. 2.) ((Vect.vz dim) /. 2.));
	    dist_between_planes = (Vect.vx dim) /. 2.;
	  };
	(* Face bas *)
	side3 =
	  {
	    plane =
	      {
		vect_unit_normal = Vect.opp Vect.yunit;
		relative_dist = 0.;
	      };
	    opposite_plane =
	      {
		vect_unit_normal = Vect.opp Vect.yunit;
		relative_dist = -. Vect.vy dim;
	      };
	    center = Vect.add position (Vect.make ((Vect.vx dim) /. 2.) 0. ((Vect.vz dim) /. 2.));
	    opposite_center =  Vect.add position
	      (Vect.make ((Vect.vx dim) /. 2.) (Vect.vy dim) ((Vect.vz dim) /. 2.));
	    dist_between_planes = (Vect.vy dim) /. 2.;
	  };
      },
     convert_texture t env_num)

  | Translate (o, v) -> 
    let obj = (convert_obj o env_num env_obj) in
    (convert_obj_translate (fst obj) (convert_vector v env_num), snd obj)

  | Scale (o, e) ->
    let obj = (convert_obj o env_num env_obj) in
    (convert_obj_scale (fst obj) (convert_expr e env_num), snd obj)

  | Rotate (o, r) ->
    let obj = (convert_obj o env_num env_obj) in
    (convert_obj_rotate (fst obj) (convert_rotation r env_num), snd obj)
  (*
    | Group l ->
    let rec aux list = match list with 
    |*)

  | _-> failwith "Les groupes ne sont pas encore gérés !"

;;

let rec convert_boolean boolean env = match boolean with
    | And (b1, b2) -> (convert_boolean b1 env) && (convert_boolean b2 env)
    | Or (b1, b2) -> (convert_boolean b1 env) || (convert_boolean b2 env)
    | Not (b) -> not (convert_boolean b env)
    | Equal (e1, e2) -> (convert_expr e1 env) = (convert_expr e2 env)
    | Less (e1, e2) -> (convert_expr e1 env) < (convert_expr e2 env)
;;

let rec convert_instruction instruction procs put_obj env_num env_obj time = match instruction with
  | SetNum (id, e) -> Environment.add env_num id (convert_expr e !env_num);
  | SetObj (id, o) -> Environment.add env_obj id (convert_obj o !env_num !env_obj);
  | Call (name, args) -> 
    convert_proc procs put_obj name (convert_expr_list args !env_num) time;
  | Put o -> put_obj := (convert_obj o !env_num !env_obj)::(!put_obj);
  | If (cond, instr_true, instr_false) ->
    let l = if (convert_boolean cond !env_num) then instr_true else instr_false in
    convert_instructions_list l procs put_obj env_num env_obj time;

and

convert_instructions_list list procs put_obj env_num env_obj time =
  match list with
    | [] -> ();
    | h::t -> 
      convert_instruction h procs put_obj env_num env_obj time; 
      convert_instructions_list t procs put_obj env_num env_obj time; 

and

convert_proc procs put_obj name args time =
  let rec find_proc l k = match l with
    | [] -> failwith ("Unknown procedure "^name)
    | h::t ->
      if k = h.name
      then h
      else find_proc t k
  in

  let proc = find_proc procs name in
  let env_num = ref (Environment.fusion proc.params args) in
  let env_obj = ref Environment.empty in
  Environment.add env_num "time" time;
  
  convert_instructions_list proc.body procs put_obj env_num env_obj time;
;;

let convert_camera camera env_num =
  {
    distance_origine = convert_expr camera.viewdist env_num;
    angle = convert_expr camera.angle env_num;
  }
;;

let convert_light_list lights env_num =
  let rec aux l = match l with
    | [] -> []
    | light::t ->
      {
	direction = Rotation.apply (convert_rotation light.l_dir env_num) Vect.yunit;
	intensity = convert_expr light.l_intensity env_num;
      }
      ::(aux t)
  in
  aux lights
;;

let convert_scenario scenario time =
  let env_global_num = ref Environment.empty in
  let env_global_obj = ref Environment.empty in
  let put_obj = ref [] in

  (* Ajout de la variable time dans l'environnement *)
  Environment.add env_global_num "time" time;

  let camera = convert_camera scenario.camera !env_global_num in
  let ambient = convert_expr scenario.ambient !env_global_num in
  let lights = convert_light_list scenario.lights !env_global_num in
   
  convert_instructions_list scenario.main scenario.procs put_obj env_global_num env_global_obj time;

  (!put_obj, camera, ambient, lights)
;;

let print_obj o = 
  let print_plane p =
    print_string "vect_unit_normal:";
    Vect.print p.vect_unit_normal;
    print_endline (Printf.sprintf ("relative_dist:%f") p.relative_dist);
  in

  let print_face f =
    print_endline "face";
    print_endline "plane 1:";
    print_plane f.plane;
    print_endline "plane 2:";
    print_plane f.opposite_plane;
    print_string "center 1:";
    Vect.print f.center;
    print_string "center 2:";
    Vect.print f.opposite_center;
    print_endline (Printf.sprintf ("dist_between_planes:%f") f.dist_between_planes);
  in

  match o with
  | (obj, texture) ->
    print_endline "----------------------------------------------";
    (match obj with 
    | Sphere sphere ->
      print_endline "SPHERE";
      print_string "center:";
      Vect.print sphere.center;
      print_endline (Printf.sprintf ("ray:%f") sphere.ray);
    | Plane plane -> 
      print_endline "PLANE";
      print_plane plane;
    | Box box -> 
      print_endline "BOX";
      print_face box.side1;
      print_face box.side2;
      print_face box.side3;);
      
    print_endline "----";
    print_endline "TEXTURE";
    print_endline (Printf.sprintf ("kd:%f\nks:%f\nphong:%f\ncolor:") texture.kd texture.ks texture.phong);
    Color.print texture.color;
    print_endline "----";
;;

let print_light light = 
  print_endline "----------------------------------------------";
  print_endline "LIGHT";
  print_string "direction:";
  Vect.print light.direction;
  print_endline (Printf.sprintf ("intensity:%f") light.intensity);
;;
