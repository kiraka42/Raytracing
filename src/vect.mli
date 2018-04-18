
(* Vecteurs dans l'espace tri-dimensionnel. Le type [vecteur] sera également
   utilisé pour représenter les points de l'espace (on confond le point
   $P$ et le vecteur $\vec{OP}$). *)

type t

(* Projections selon les trois axes. *)
val vx : t -> float
val vy : t -> float
val vz : t -> float

(* Construction d'un vecteur. *)
val make : float -> float -> float -> t

(* Les trois vecteurs unitaires de la base. *)
val xunit : t
val yunit : t
val zunit : t

(* Produit scalaire u.v. Propriété utilisée par la suite :
   Si alpha est l'angle que font les vecteurs u et v alors
   u.v = cos(alpha)*|u|*|v|. *)
val scalprod : t -> t -> float

(* Norme |u| d'un vecteur u. *)
val norm : t -> float

(* Somme de vecteurs. *)
val add : t -> t -> t

(* Différence de vecteurs. *)
val diff : t -> t -> t

(* Opposé d'un vecteur. *)
val opp : t -> t

(* Multiplication d'un vecteur par un scalaire. *)
val shift : float -> t -> t

(* Distance entre deux points représentés chacun par son vecteur
   depuis l'origine. *)
val dist : t -> t -> float

(* Construction d'un vecteur unitaire de même direction qu'un vecteur
   non-nul donné. *)
val normalise : t -> t

(* Fonctions spécialisées : elles peuvent être ignorées dans un premier
   temps, mais pourront aider ensuite à améliorer légèrement les performances
   si on les code efficacement. *)

val dist2 : t -> t -> float (* carré de la distance entre deux points *)
val normalised_diff : t -> t -> t (* un diff puis un normalise *)

(* Eventuellement, pour du debug: *)
val print : t -> unit
