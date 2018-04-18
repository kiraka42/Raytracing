open Graphics
open Screen
open Camera
open Convert
open Intersection

(*
  TEST : print de la liste des objets
*)
let print_scene list screen camera lights = 
  let rec aux_obj l = match l with
    | [] -> ();
    | h::t -> print_obj h; aux_obj t
  in
  let rec aux_light l = match l with
    | [] -> ();
    | h::t -> print_light h; aux_light t
  in

  print_endline (Printf.sprintf ("Screen => distance_camera:%f, largeur:%d, hauteur:%d")
  screen.distance_camera screen.l screen.h);
  print_endline (Printf.sprintf ("Camera => distance_origine:%f, angle:%f")
  camera.distance_origine camera.angle);
  aux_obj list;
  aux_light lights
(* 
   read_scenario : Lecture du scenario.
*)
let read_scenario f =
  let buf = open_in f in
  let lexbuf = Lexing.from_channel buf in
  try
    let (sc:Scenario.scenario) = Parse.scenario Lex.next_token lexbuf in
    close_in buf;
    sc
  with e ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "File %s, line %d, character %d: parse error\n"
                  f pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    flush stderr;
    close_in buf;
    raise e

let output_image im file_name =
  let oc = open_out_bin file_name in
  let imv = dump_image im in
    output_value oc imv;
    close_out oc
;;


(* [save_screen] : fonction qui sauvegarde le contenu actuel de la fenêtre 
 * graphique dans le fichier [file_name]. *)
let save_screen file_name =
  let im = get_image 0 0 (size_x ()) (size_y ()) in
    output_image im file_name
    ;;
    
(* 
   fill_graph : Rempli le graph en récupérant les couleurs adéquates à chaque
   pixel.
*)
let fill_graph screen list_objects camera ambient lights =

  let position_camera = Vect.make 0. 0. camera.distance_origine in
  let z = camera.distance_origine -. screen.distance_camera in

  for i = 0 to screen.l do
    for j = 0 to screen.h do
      let direction = Vect.normalise 
	(Vect.make 
	   (float_of_int (i-(screen.l/2))) 
	   (float_of_int j) 
	   z
	)
      in
      let ray = create_ray position_camera direction in
      let color_pixel = color_view_by_ray ray !Options.depth list_objects ambient lights in
      moveto i j;
      set_color (Color.to_graphics color_pixel);
      plot i j;
    done
  done

(* MAIN *)
let main () =
  (* lecture des options *)
  Options.parse();
  
  (* Lecture du scenario *)
  if !Options.filename = "" then
    begin
      print_endline ("Usage: "^Options.usage_msg);
      exit 1;
    end;
  let scenario = read_scenario !Options.filename in

  (* Convertion du scenario en scene 3D *)
  let (list_objects, camera, ambient, lights) = 
    convert_scenario scenario (float_of_int !Options.anim) in
  let screen = Screen.make !Options.hsize !Options.vsize camera in

  print_scene list_objects screen camera lights;

  (* Ouverture du graph *)
  open_graph (Printf.sprintf " %dx%d" !Options.hsize !Options.vsize);

  (* Dessin *)
  fill_graph screen list_objects camera ambient lights;

  (* Fermeture seulement après avoir cliqué *)
  ignore (Graphics.read_key ());
  exit 0

let _ =
  if not !Sys.interactive then main ()
