(*******************************
       Projet AP3

	GHITA Alessandro
	GELINAUD Clement
	BENMOUSSATI Souhail
	
********************************)


(*************** Mise en place de la bibliothèque graphique ***************)

(* nécessaire pour les versions >= 4.09 *)
#use "topfind";;
#require "graphics";;
(****************************************)

(* On charge la bibliothèque graphique dans l'interpréteur
		et on ouvre le module *)
#load "graphics.cma";;
open Graphics;;



(*************** Les boucles ***************)

let rec forloop(r, n, next : 'a * int* ('a -> 'a)) : 'a =
  if n = 0 then r
  else forloop (next(r), n-1, next)
;;

let rec whileloop(r, cont, next : 'a * ('a -> bool) * ('a -> 'a)): 'a =
  if not(cont(r)) then r
  else whileloop(next(r), cont, next)
;;

(*************** Exemples d'utilisation des boucles ***************


(forloop((1, 1),
         5,
         (function (i, k) -> print_int i; 
                             print_newline (); 
                             (i+1, i*k)
         )
   )
)
;;

let affiche_compteurs(n, m : int * int) : unit =
  snd (forloop((0, ()),
               n,
               (function (i,()) -> snd (forloop((0, ()),
                                                m,
                                                (function (j, ()) -> print_int i; 
					                             print_string " "; 
					                             print_int j; 
					                             print_newline (); (j+1, ()))));
                                   (i+1, ())))
    )
;;

affiche_compteurs 10 5
;;

affiche_compteurs 5 10
;;

whileloop((0,[]), (function (n, l) -> n < 10),
  (function (n, l) -> (n+1, n::l)))
;;


let square_root a = 
  whileloop(1.0,
            (function x -> abs_float( x *. x -. a) >= 10. ** (-.10.)),
              (function x -> (0.5 *. (x +. a /. x))))
;;

square_root 2.
;;

********************************)


(*************** Les types pour les images ***************)

type couleur = Noir | Blanc
;;

type picture = couleur array array
;;

type arbre = Feuille of couleur 
	     | Noeud of arbre * arbre * arbre * arbre
;;


(*************** Les fonctions draw_picture et read_pbm ***************)

(* Dessine une image donnée comme un tableau de couleurs *)
let draw_picture img = 
  let size = (Array.length img) in
    resize_window size size ;
    snd (
        forloop((0, ()),
                size,
	        (function (i, ()) 
	                  -> ( snd (forloop((0, ()),
                                            size,
			                    (function (j, () ) 
			                              -> ( set_color ( match img.(i).(j) with 
			                                               | Blanc -> white 
			                                               | Noir -> black ) ;
				                           plot i j ;
				                           (j+1, ()) )
			                    )
                                 )) ;
		               (i+1, ()) )
	        )
          )
      )
;;

(* Lecture d'un fichier pbm au format ascii *)
let read_pbm filename =
  let file = open_in filename in
  let magic = input_line file in
    if magic <> "P1" 
    then 
      failwith "ce n'est pas un fichier pbm ascii"
    else
      let size_line = whileloop( (input_line file),
                                 (function l -> (l.[0] = '#' || l.[0] = ' ')),
	                         (function l -> input_line file))
	                 
      in
      let space_index = (String.index size_line ' ') in 
      let size = int_of_string (String.sub size_line 0 space_index) 
      and other_size = int_of_string (String.sub size_line (space_index + 1)
					 ((String.length size_line) - space_index - 1))
      in
	if (size <> other_size) 
	then
	  failwith "ce n'est pas une image carrée"
	else
	  let img = Array.make_matrix size size Blanc in
	    snd (
	      try (
		whileloop((0, ()),
                          (function (k, ()) -> true),
		            (function (k, ()) -> ( match (input_char file) with
	                                           | '0' -> (k+1, ())
	                                           | '1' -> (img.(k mod size).(size - (k / size) - 1) <- Noir; 
						             (k+1, ()))
	                                           | _   -> (k, ())
                                                 )
		            )
                  )
	      )
	      with End_of_file -> (0, ()) 
	    ) ;
	    close_in file ; 
	    img
;;


(*********************************
     Exemples d'utilisation 
     des fonctions read_pbm
     et draw_pbm
*********************************)
let d = read_pbm "portrait.pbm"
;;			 

open_graph ""
;;

draw_picture d
;;

draw_picture (read_pbm "avion.pbm")
;;

close_graph() 
;;


(*************** Mettez ici vos réponses aux questions et fonctions ***************)

(* Question 1 *)
let rec is_puiss_2(n : int) : bool =
	match n with
	| 0 -> false
	| 1 -> true
	| _ -> (
		match (n mod 2) with
		| 0 -> is_puiss_2(n / 2)
		| _ -> false
	)
;;


(* Question 2 *)
let img_test = [|
  [| Blanc; Noir; Blanc; Blanc |];
  [| Noir; Blanc; Blanc; Blanc |];
  [| Noir; Noir; Blanc; Noir |];
  [| Noir; Noir; Noir; Blanc |]
|]
;;

let img_test1 = [|
  [| Noir; Noir; Blanc; Blanc |];
  [| Noir; Noir; Blanc; Blanc |];
  [| Noir; Noir; Noir; Noir |];
  [| Noir; Noir; Noir; Noir |]
|]
;;

let arb_img_test = Noeud(
                        Noeud(Feuille(Blanc), Feuille(Noir), Feuille(Noir), Feuille(Blanc)),
                        Feuille(Blanc),
                        Feuille(Noir),
                        Noeud(Feuille(Blanc), Feuille(Noir), Feuille(Noir), Feuille(Blanc))
                     );;

let arb_img_test1 = Noeud(
                        Feuille(Noir),
                        Feuille(Blanc),
                        Feuille(Noir),
                        Feuille(Noir)
                      );;


(**************)
(* Question 3 *)
(**************)

let random_img(taille, nombrePixelNoir : int * int) : picture =
	if not(is_puiss_2(taille))
  then failwith ("ERROR random_img : The size must be a power of 2")
  else
    if nombrePixelNoir > (taille * taille)
    then failwith("ERROR random_img : Number of pixels is too large")
    else
    	let image : picture = Array.make_matrix taille taille Blanc in
    	
    	let (compteur, imageFinal) : int * picture =
    	forloop(
    		(0, image),
    		nombrePixelNoir,
    		(function (i, image) ->
    			let randomX = Random.int(taille) and randomY = Random.int(taille) in
    				image.(randomX).(randomY) <- Noir;
    				(i + 1, image);
    		)
			)
      in imageFinal
;;

open_graph "";;
draw_picture(random_img(512, 512 * 512));;
draw_picture(random_img(512, 1000));;
close_graph();;


(****************)
(* Question 4.1*)
(****************)
(*
let image_vers_arbre k img =
*)
(* 
Exemples d'utilisation 
de la fonction image_vers_arbre

*)

(****************)
(* Question 4.2 *)
(****************)
(*
let remplir_carre img i j k c =
*)
(* 
Exemples d'utilisation 
de la fonction remplir_carre

*)

(*
let arbre_vers_image k arb =
*)
(* 
Exemples d'utilisation 
de la fonction arbre_vers_image

*)

(**************)
(* Question 5 *)
(**************)
(*
let draw_tree k arb =
*)
(* 
Exemples d'utilisation 
de la fonction draw_tree

*)

(****************)
(* Question 6.1 *)
(****************)

(*
Exemples d'agrandissement d'images

*)

(****************)
(* Question 6.2 *)
(****************)

(*
let rotation arb = 
*)
(* 
Exemples d'utilisation 
de la fonction rotation

*)

(**************)
(* Question 7 *)
(**************)
(*
let fractale k n =
*)
(* 
Exemples d'utilisation 
de la fonction fractale

*)

(****************)
(* Question 8.1 *)
(****************)
(*
let arbre_vers_bits arb =
*)
(* 
Exemples d'utilisation 
de la fonction arbre_vers_bits

*)

(****************)
(* Question 8.2 *)
(****************)
(*
let bits_vers_octets lb =
*)
(* 
Exemples d'utilisation 
de la fonction bits_vers_octets

*)

(****************)
(* Question 8.3 *)
(****************)
(*
let bits_vers_arbres lb =
*)
(* 
Exemples d'utilisation 
de la fonction bits_vers_arbres

*)
(*
Explications sur la gestion 
des erreurs de la fonction bits_vers_arbres
*)

(*
let octet_vers_bits lo =
*)
(* 
Exemples d'utilisation 
de la fonction octet_vers_bits

*)

(****************)
(* Question 8.4 *)
(****************)
(*
 let write_arbre filename arb =
*)
(* 
Exemples d'utilisation 
de la fonction write_arbre

*)

(*
let read_arbre filename =
*) 
(* 
Exemples d'utilisation 
de la fonction read_arbre

*)



