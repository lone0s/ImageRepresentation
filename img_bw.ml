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


(* Question 4.1 *)

let arbre_vers_image k arb =
	let imgResult = Array.make_matrix k k Blanc in
	let rec arbre_vers_image_aux img x y k arb_aux =
		match arb_aux with
		|Feuille (leafColor) -> remplir_carre imgResult  x y k leafColor
		|Noeud(c1,c2,c3,c4) ->
		let k2 : int =(k/2)
		in
			(
				arbre_vers_image_aux img x (y+k2) k2 c2 ;
				arbre_vers_image_aux img (x+k2) (y+k2) k2 c1 ;
				arbre_vers_image_aux img x y k2 c4;
				arbre_vers_image_aux img (x+k2) y k2 c2;
			)
			in (
			arbre_vers_image_aux imgResult 0 0 k arb);
			imgResult;
			;;
(* Possibilité de casser la fonction parce que pas de condition d'arret explicitée *)


(* Exemples d'utilisation de la fonction remplir_carre *)
arbre_vers_image 4 arb_img_test1;;
arbre_vers_image 4 arb_img_test;;

(* -- Fin Exemples Remplir Carre-- *)

(* Pour une raison qui m'échappe, ordre de parcours est inversé si je met C1/C2/C3/C4 dans le bon ordre*)
(* A revoir *)
let image_vers_arbre k img =
  let rec img_vers_arb_aux x y k=
    if k <= 1 then
      Feuille img.(x).(y)
    else
      let k2 = k/2 in
      let c2 = img_vers_arb_aux x (y+k2) k2
      and c1 = img_vers_arb_aux (x+k2) (y+k2) k2
      and c4 = img_vers_arb_aux x y k2
      and c3 = img_vers_arb_aux (x+k2) y k2
      in
      match c1, c2, c3, c4 with
        | Feuille f1, Feuille f2, Feuille f3, Feuille f4
          when f1 == f2 && f2 == f3 && f3 == f4 -> c1
        | _,_,_,_ -> Noeud(c1, c2, c3, c4)
    in
  img_vers_arb_aux 0 0 ;;

	image_vers_arbre 4 img_test1;;


(* Question 4.2 *)

let remplir_carre img i j k c =

	for x = 0 to (k-1)
	do
		for y = 0 to (k-1)
		do
			img.(x+i).(y+j) <- c ;
		done ;
		done ;
		img
		;;
(* Exemples d'utilisation de la fonction remplir_carre *)
let carre_noir = [|
    [| Noir; Noir; Noir; Noir|];
	[| Noir; Noir; Noir; Noir|];
	[| Noir; Noir; Noir; Noir|];
	[| Noir; Noir; Noir; Noir|];
|]
;;
remplir_carre carre_noir 2 0 2 Blanc;;
remplir_carre carre_noir 1 0 1 Noir;;

let carre_noir_2 = Array.make_matrix 4 4 Noir ;;
remplir_carre carre_noir_2 2 0 1 Blanc;;

(* -- Fin Exemples Remplir Carre-- *)


(* Question 5 *)

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

type bit = Zero | Un;;

let rec arbre_vers_liste imageArbre liste =
	match imageArbre with
	| Feuille Blanc -> Zero :: Zero :: liste
	| Feuille Noir -> Zero :: Un :: liste
	| Noeud (sous_arbre_1, sous_arbre_2, sous_arbre_3, sous_arbre_4) ->
		Un :: arbre_vers_liste sous_arbre_1
			(arbre_vers_liste sous_arbre_2
				(arbre_vers_liste sous_arbre_3
					(arbre_vers_liste sous_arbre_4 liste)))
;;

let arbre_vers_bits arb = arbre_vers_liste arb [];;

(* Exemples d'utilisation de la fonction arbre_vers_bits *)
arbre_vers_bits arb_img_test;;


(* Question 8.2 *)

(*********************************** Fonctions auxiliaires ***********************************)
let rec recuperation_n_element_liste(liste, compteur : 'a list * int) : 'a list =
	if compteur > List.length(liste)
	then failwith("Erreur recuperation_n_element_liste() : le compteur est supérieur à la longueur de la liste.")
	else
		if compteur <= 0
		then []
		else List.hd(liste) :: recuperation_n_element_liste(List.tl(liste), compteur - 1)
;;

let rec supprimer_n_elemment_liste(liste, compteur : 'a list * int) : 'a list =
	if compteur > List.length(liste)
	then failwith("Erreur supprimer_n_elemment_liste() : le compteur est supérieur à la longueur de la liste.")
	else
		if compteur <= 0
		then liste
		else supprimer_n_elemment_liste(List.tl(liste), compteur - 1)
;;

let rec puissance(nombre, valeurPuissance : int * int) : int =
	match valeurPuissance with
	| 0 -> 1
	| valeurPuissance -> nombre * (puissance(nombre, valeurPuissance - 1))
;;

(* Utilisée pour le débuggage *)
let affichage_bit(char : bit) : string =
	match char with
	| Zero -> "Zero"
	| Un -> "Un"
;;

(*********************************************************************************************)

(****************************** Tests des fonctions auxiliaires ******************************)
let liste_test = [1;2;3;4;5];;

recuperation_n_element_liste(liste_test, 0);;
recuperation_n_element_liste(liste_test, 1);;
recuperation_n_element_liste(liste_test, 3);;
recuperation_n_element_liste(liste_test, 5);;
recuperation_n_element_liste(liste_test, 6);;

supprimer_n_elemment_liste(liste_test, 0);;
supprimer_n_elemment_liste(liste_test, 1);;
supprimer_n_elemment_liste(liste_test, 3);;
supprimer_n_elemment_liste(liste_test, 5);;
supprimer_n_elemment_liste(liste_test, 6);;

puissance(2, 0);;
puissance(2, 1);;
puissance(2, 2);;
puissance(2, 5);;

affichage_bit(Un);;
affichage_bit(Zero);;
(***************************************************)

let rec bits_vers_octets lb =
	if lb = []
	then []
	else
		let longueur_liste : int = List.length(lb) in
		
		if longueur_liste > 8
		then bits_vers_octets(recuperation_n_element_liste(lb, 8)) @
					bits_vers_octets(supprimer_n_elemment_liste(lb, 8))
		else
		(
			let nombre : int ref = ref 0 and
					liste : bit list ref = ref lb in
					
			for indice = (longueur_liste - 1) downto 0
			do
			(
				if List.hd(!liste) = Un
				then nombre := !nombre + puissance(2, indice);
				liste := List.tl(!liste);
			)
			done;
		
			[!nombre];
		
		)
;;
#trace bits_vers_octets;;
(* Exemples d'utilisation de la fonction bits_vers_octets *)

(* On récupère un arbre sous forme de liste de bits *)
let arbre_bits = arbre_vers_bits arb_img_test;;
(* On applique bits_vers_octets() *)
let arbre_octet = bits_vers_octets(arbre_bits);;


(* Question 8.3 *)

(* Converti un nombre en tableau de bits *)
let entier_vers_Liste_Bit i =

	let rec int_to_bit acc i =
	
		if i = 0
		then acc
    else int_to_bit (i land 1::acc) (i lsr 1)
    
  in
  let l = int_to_bit [] i in
  Array.of_list  l
;;

(* Convertie un tableau en 1 dimension en liste *)
let tableau_vers_liste(tableau : 'a array) : 'a list =

	let liste : 'a list ref = ref [] in

	for indice = 0 to Array.length(tableau) - 1
	do
	(
		liste := !liste @ [(Array.get tableau indice)];
	)
	done;
	
	!liste;
;;

(* Convertie une int list en bit list *)
let rec int_list_vers_bit_list(liste : int list) : bit list =
	match liste with
	| [] -> []
	| 0 :: element -> Zero :: int_list_vers_bit_list(List.tl(liste))
	| 1 :: element -> Un :: int_list_vers_bit_list(List.tl(liste))
	| _ -> failwith("Erreur int_list_vers_bit_list() : la liste n'est pas une liste de bits")
;;

(* Chaîne de bits en octet complet (sous forme de bits => ajoute des 0 si nécessaire *)
let octet(liste : int list) : int list =
	let resultat : int list ref = ref liste and
	longueur : int = List.length(liste) in

	if longueur >= 8
	then liste
	else
	(
		for indice = longueur to 7
		do resultat := [0] @ ! resultat;
		done;
		!resultat;
	)
;;

let rec liste_octet_vers_liste_nombre(liste_octet : int list) : bit list =
	if liste_octet = []
	then []
	else
		let liste : int list ref = ref (tableau_vers_liste(entier_vers_Liste_Bit (List.hd(liste_octet)))) in
		
		let longueur : int = List.length(!liste) in
		
		if longueur <> 1
		then liste := octet(!liste);
	
		int_list_vers_bit_list(!liste) @ liste_octet_vers_liste_nombre(List.tl(liste_octet))
;;

let octet_vers_bit lo =
	liste_octet_vers_liste_nombre(lo)
;;

(* Exemples d'utilisation de la fonction octet_vers_bits *)

(* On rappelle les variables *)
arbre_octet;;
arbre_bits;;
(* On vérifie que l'on obtient bien le même résultat que arbre_bits *)
octet(tableau_vers_liste(entier_vers_Liste_Bit 6));;
octet_vers_bit arbre_octet;;
let test_arbre_octet_vers_bit = ((octet_vers_bit arbre_octet) = arbre_bits);;


let bits_vers_arbres lb =
;;




(*
   La mission de do_parse l est
   * Si l est de la forme la @ rem où la représente un arbre a,
     alors renvoie la paire a,rem.

   * Sinon, fais ce que dois, advienne que pourra
     (cf. assert false, devise de Du Guesclin)
*)

let rec do_parse l =
	match l with
	| Zero :: Zero :: rem -> Feuille Blanc, rem
	| Zero :: Un :: rem -> Feuille Noir, rem
	| Un :: rem ->
    let a1,rem = do_parse rem in
    let a2,rem = do_parse rem in
    let a3,rem = do_parse rem in
    let a4,rem = do_parse rem in
    Noeud (a1,a2,a3,a4),rem
	| _ -> failwith("Erreur")
;;

let liste_vers_arbre l = let a,_ = do_parse l in a
;;

arbre_octet;;
octet_vers_bit arbre_octet;;
liste_vers_arbre (octet_vers_bit arbre_octet);;




(* 
Exemples d'utilisation 
de la fonction bits_vers_arbres

*)
(*
Explications sur la gestion 
des erreurs de la fonction bits_vers_arbres
*)




(* Question 8.4 *)

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



