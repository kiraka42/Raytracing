(* module pour les fichiers d'image en format PPM *)
(**************************************************)

(* Pour plus de détail, voir par exemple:
   - https://fr.wikipedia.org/wiki/Portable_pixmap
   - http://netpbm.sourceforge.net/doc/ppm.html
*)

(* Canal de sortie associé à un fichier PPM. *)
type ppm_file_channel

(* (openfile hsize vsize name) ouvre le fichier name.ppm pour
   l'écriture et y écrit l'entête PPM pour une image de taille hsize et
   de hauteur vsize.
   Selon l'option "binary", ce fichier ppm sera binaire (de type "P6")
   ou ascii (de type "P3").
   Lève l'exception Error quand l'ouverture du fichier echoue. *)
val openfile : ?binary:bool -> int -> int -> string -> ppm_file_channel
exception Error of string

(* (put_next_pixel c (r,g,b)) ajoute le point (r,g,b) au canal c.
   r, g et b doivent être des valeurs entières entre 0 et 255 qui donnent la
   luminosité du point dans les couleurs rouge, vert et bleu. *)
val put_next_pixel : ppm_file_channel -> int * int * int -> unit

(* Fermer un canal PPM  *)
val closefile : ppm_file_channel -> unit
