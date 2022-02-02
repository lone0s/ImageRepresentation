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
    		
    			let randomX : int ref = ref 0 and
    					randomY : int ref = ref 0 and
    					stop : bool ref = ref false in

    			while not(!stop) do
    			(
    				randomX := Random.int(taille);
    				randomY := Random.int(taille);
    				
						if image.(!randomX).(!randomY) <> Noir
						then stop := true;
					)
					done;

    			image.(!randomX).(!randomY) <- Noir;
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

let rec dessine i j k = function
  | Feuille Noir -> Graphics.fill_rect i j k k
  | Feuille Blanc -> ()
  | Noeud (c1, c2, c3, c4) ->
		let k2 = k/2 in
			dessine i (j+k2) k2 c1;
      dessine (i+k2) (j+k2) k2 c2;
      dessine i j k2 c3;
      dessine (i+k2) j k2 c4
;;

let draw_tree(imageArbre, taille :  arbre * int) : unit =
	resize_window taille taille ;
	dessine 0 0 taille imageArbre;
;;

open_graph "";;
draw_tree(arb_img_test, 500);;
clear_graph ();;
close_graph();;


(* Question 6.1 *)

(* Exemples d'agrandissement d'images *)


(* Question 6.2 *)

let rec rotation arbre =
	match arbre with
	| Feuille _ -> arbre
	| Noeud (noeud1, noeud2, noeud3, noeud4) ->
    	Noeud (rotation noeud3, rotation noeud1, rotation noeud4, rotation noeud2)
;;

(* Exemples d'utilisation de la fonction rotation *)
open_graph "";;
draw_tree(arb_img_test, 500);;
clear_graph ();;
draw_tree(rotation arb_img_test, 500);;
close_graph();;


(* Question 7 *)
(*
let fractale k n =
*)
let rec rotation_gauche arbre =
	match arbre with
	| Feuille _ -> arbre
	| Noeud (noeud1, noeud2, noeud3, noeud4) ->
    	Noeud (rotation_gauche noeud2, rotation_gauche noeud4, rotation_gauche noeud1, rotation_gauche noeud3)
;;

let rec fractale(taille, nombre_iteration : int * int) : arbre =
  if nombre_iteration <= 0
  then Feuille Noir
  else
    let c = fractale (taille, nombre_iteration - 1) in
    let c1 = Noeud (c, c, c, Feuille Blanc) in
    let c3 = rotation_gauche c1 in
    let c4 = rotation_gauche c3 in
    let c2 = rotation_gauche c4 in
    Noeud (c1, c2, c3, c4)
;;

(* Exemples d'utilisation de la fonction fractale *)
(* ne pas dépasser les 4 itérations *)
open_graph "";;

draw_tree(fractale(512, 3), 512);;

clear_graph ();;
close_graph();;


(* Question 8.1 *)

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



