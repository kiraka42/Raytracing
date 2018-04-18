val convert_expr : Scenario.expr -> float Environment.t -> float

val convert_expr_list : Scenario.expr list -> float Environment.t -> float list

val convert_color : Scenario.color -> float Environment.t -> Color.t

val convert_rotation : Scenario.rotation -> float Environment.t -> Rotation.t

val convert_vector : Scenario.vector -> float Environment.t -> Vect.t

val convert_position : Scenario.position -> float Environment.t -> Vect.t

val convert_texture : Scenario.texture -> float Environment.t -> Object.texture

val convert_plane_translate : Object.plane -> Translation.t -> Object.plane

val convert_face_translate : Object.face -> Translation.t -> Object.face

val convert_obj_translate : Object.obj -> Translation.t -> Object.obj

val convert_plane_scale : Object.plane -> Scaling.t -> Object.plane

val convert_face_scale : Object.face -> Scaling.t -> Object.face

val convert_obj_scale : Object.obj -> Scaling.t -> Object.obj

val convert_plane_rotate : Object.plane -> Rotation.t -> Object.plane

val convert_face_rotate : Object.face -> Rotation.t -> Object.face

val convert_obj_rotate : Object.obj -> Rotation.t -> Object.obj

val convert_obj : Scenario.obj -> float Environment.t -> Object.t Environment.t -> Object.t

val convert_boolean : Scenario.boolean -> float Environment.t -> bool

val convert_instruction : Scenario.instruction -> Scenario.proc list -> Object.t list ref -> float Environment.t ref -> Object.t Environment.t ref -> float -> unit

val convert_instructions_list : Scenario.instruction list -> Scenario.proc list -> Object.t list ref -> float Environment.t ref -> Object.t Environment.t ref -> float -> unit

val convert_proc : Scenario.proc list -> Object.t list ref -> string -> float list -> float -> unit

val convert_camera : Scenario.camera -> float Environment.t -> Camera.t

val convert_light_list : Scenario.light list -> float Environment.t -> Light.t list

val convert_scenario : Scenario.scenario -> float -> (Object.t list * Camera.t * float * Light.t list)

val print_obj : Object.t -> unit

val print_light : Light.t -> unit
