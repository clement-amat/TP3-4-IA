open List;;

(*****************************************************************************)
(* Des graphes : *************************************************************)
(*****************************************************************************)

(* Un petit graphe en extension : *)

let g1 = ["WA","NA" ; "WA","SA" ; "NA","SA" ; "NA","Q" ; "SA","Q" ; "SA","V" ; "SA","NSW" ; "Q","NSW" ; "NSW","V" ]


(* Un autre petit graphe pour vérifier le retour arrière *)

let g2 = ["A","B" ; "A","E" ; "B","C" ; "B","D" ; "B","E" ; "C","D" ; "D","E"]

(* Construction à partir d'un fichier au format "DIMACS standard" - décommenter les 4 dernières lignes *)

let rec split s = try let i = String.index_from s 0 ' '
	in (String.sub s 0 i)::(split (String.sub s (i+1) ((String.length s )-(i+1))))
	with Not_found -> [s]

let lire_graphe (nom_fichier : string) : ('a * 'a) list =
(* retourne la liste des arêtes *)
	let ref_l_aretes = ref [] and flux = open_in nom_fichier
	in try
		while true; do match (split (input_line flux)) with
			  "e"::x::y::[] -> ref_l_aretes := (int_of_string x, int_of_string y)::!ref_l_aretes
			| _ -> ()
  		done ; !ref_l_aretes
		with End_of_file -> close_in flux ; !ref_l_aretes


(* 
let myciel3 = lire_graphe "myciel3.col" (* 11 sommets *)
let myciel4 = lire_graphe "myciel4.col" (* 23 sommets , 5 couleurs *)
let myciel5 = lire_graphe "myciel5.col" (* 47 sommets *)
let miles750 = lire_graphe "miles750.col" (* 128 sommets *)
*)

(**
            PROGRAMMES
 *)

let pop = function [] -> failwith "pas pop" | t::r -> t;;
let tail = function [] -> [] | t::r -> r;;

let rec appartient = fun elt -> fun liste ->
	match liste with
	[] -> false
	| a::l -> a = elt || (appartient elt l);;

let rec union = fun ensemble1 -> fun ensemble2 ->
	match ensemble1 with 
	  [] -> ensemble2
	| a::l -> if (appartient a ensemble2) then (union ensemble2 l) else a::(union ensemble2 l);;
	
let rec inclus = fun ensPrincipal -> fun ensInclus ->
	match ensInclus with
	 [] -> true
	| a::l -> (appartient a ensPrincipal) && (inclus ensPrincipal l);;

let rec egalite = fun ensemble1 -> fun ensemble2 ->
	(inclus ensemble1 ensemble2) && (inclus ensemble2 ensemble1);;
	
(** Savoir si deux somments sont adjacents *)
let sontAdjacents (graphe : ('a * 'a) list) (sommetA : 'a) (sommetB : 'a) : bool =
    List.exists (fun (a, b) -> (a = sommetA && b = sommetB) || (a = sommetB && b = sommetA)) graphe;;

(** Sommets du graphes *)
let rec sommets (graphe : ('a * 'a) list) : 'a list =
    match graphe with
    | (a, b)::reste -> union [a; b] (sommets reste)
    | _ -> [];;
    
(** Voisins d'un somment *)
let listeVoisins (graphe : ('a * 'a) list) (sommet : 'a) : 'a list =
    let sommets = (sommets graphe) in
    let rec aux = fun s ->
        match s with
        | t::r -> if (sontAdjacents graphe t sommet) then t::(aux r) else (aux r)
        | [] -> []
    in aux sommets;;
    
(** Liste des couleurs utilisées pour colorer les voisins d'un sommet *)
let rec listeCouleurs (graphe : ('a * 'a) list) (colPartielle : ('a * 'int) list) (s : 'a) : (int list) =
    match colPartielle with
      (som, col):: reste -> if (sontAdjacents graphe s som) 
                            then col::(listeCouleurs graphe reste s) 
                            else (listeCouleurs graphe reste s)
    | [] -> [];;
    
(** Plus petit entier non encore utilisé dans une liste d'entiers *)    
let minNonUtilise = fun l -> 
    let rec trouver = fun entier l -> match l with
    | []      -> entier + 1
    | x::lbis -> if x = entier + 1 
                 then trouver x lbis
                 else entier + 1 in 
                 trouver 0 (List.sort_uniq (fun x y -> x-y) l);;
                                
(** Vrai s'il reste des sommets à coloriers en fonction de
    la liste des sommets du graphe et de la coloration partielle *)  
let resteSommetsAColorier (lesSommets : 'a list) (colPartielle : ('a * int) list) : bool =
    match (List.split colPartielle) with
    [], _ -> true
    | l1, l2 -> not (egalite l1 lesSommets);;   

(** Vrai si colPartielle contient c *)
let couleurDejaUtilisee = fun colPartielle c ->
    match (List.split colPartielle) with
    | ([], _) -> false
    | (listeSommets, listeCouleurs) -> List.exists (fun p -> p = c) listeCouleurs;;
              
(**
         ALGORITHME GLOUTON SANS UTILISER D'HEURISTIQUES
 *)               
let gloutonSansH (graphe : ('a * 'a) list) = 
    let rec aux (coul:int) (colPartielle: ('a * int) list) (nbCouleurs:int) (sommetsAColorer:'a list) (sommetsColores:'a list) =
        if (resteSommetsAColorier (sommets graphe) colPartielle) 
        then
          (let s = (pop sommetsAColorer) in
           let nouvCoul =  minNonUtilise (listeCouleurs graphe colPartielle s) in
           aux (if nouvCoul > coul then nouvCoul else coul)
               (colPartielle@[(s, nouvCoul)])
               (if (couleurDejaUtilisee colPartielle nouvCoul) then nbCouleurs + 1 else nbCouleurs)
               (tail sommetsAColorer)
               (sommetsColores@[s]))
        else 
            colPartielle, nbCouleurs, sommetsColores
     in aux 1 [] 0 (sommets graphe) [];;                
                     
                       
            
            
         
    
    

