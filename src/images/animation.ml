(*simple fichier  ml  qui permet d'afficher a la suite plusieurs images capturé*)
open Graphics;;


let usage = 
  "Usage: " ^ Sys.argv.(0) ^ " [nombre_images] [nombre_millisecondes]\n"
;;


if Array.length Sys.argv != 3
then (print_string usage; exit 1)
;;


let mysleep n =
  let start = Unix.gettimeofday() in
  let rec delay t =
    try
      ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. n -. now in
	if remaining > 0.0 then delay remaining
  in delay n
;;

(* [nb_images] : nombre d'images à afficher récupéré en ligne de commande *)
let nb_images = 
  try
    int_of_string (Sys.argv.(1))
  with
    | Failure(s) -> (print_string "Erreur: le nombre d'images doit être un nombre entier strictement positif\n"; exit 1)
;;


let nb_ms = 
  try
    int_of_string (Sys.argv.(2))
  with
    | Failure(s) -> (print_string "Erreur: le nombre de millisecondes doit être un nombre entier positif\n"; exit 1)
;;


if nb_images <= 0 
then (print_string "Erreur: le nombre d'images doit être strictement positif\n"; exit 1)
else if nb_ms < 0
then (print_string "Erreur: le nombre de millisecondes doit être positif\n"; exit 1)
;;


open_graph " 400x200";;


let input_image file_name =
  let ic = open_in_bin file_name in
  let im = make_image (input_value ic) in
    close_in ic;
    im
;;


let restore_screen file_name =
  let im = input_image file_name in
    draw_image im 0 0
;;


for i=1 to nb_images do
  try
    let fichier = "image" ^ (string_of_int(i)) in
      restore_screen fichier;
      mysleep ((float_of_int nb_ms) /. 1000.) (* on attend [nb_ms] millisecondes avant d'afficher l'image suivante *)
  with Sys_error(s) -> Printf.printf "Erreur d'ouverture du fichier img/image%d\n" i
done
;;

