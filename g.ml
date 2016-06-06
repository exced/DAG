open Graph.Pack.Digraph;;
open DAG;;


(* ------------------------ Affichage ------------------------ *)
let printSommets g = fold_vertex (fun v qt -> Format.printf "%s " (DAG.Display.vertex_name v)) g ();;
let printSommetsMark g = fold_vertex (fun v qt -> Format.printf "(%s ,%i) " (DAG.Display.vertex_name v) (Mark.get v)) g ();;
let printAretes g = fold_edges (fun v1 v2 qt-> Format.printf "- %s %s -" (DAG.Display.vertex_name v1) (DAG.Display.vertex_name v2)) g ();;
let printListeSommets l = List.fold_right (fun v qt -> Format.printf "%s " (DAG.Display.vertex_name v)) l ();;
let printListeListeSommets l = List.map (fun v -> begin Format.printf "[ "; (printListeSommets v); Format.printf " ] " end ) l;;
let printMarkfromList l = List.fold_right (fun v qt -> Format.printf "%i " (Mark.get v)) l ();; 


(* ------------------------ sansDep ------------------------ *)
(* sans dependances
	entree: graphe
	sortie: liste des vertex sans dependances de g
val sansDep : Graph.Pack.Digraph.t -> Graph.Pack.Digraph.V.t list 
*)
let sansDep g =
	fold_vertex (fun t q -> if ((pred g t) = [] ) then t::q else q) g []
;;


(* ------------------------ Manipulation listes ------------------------ *)
(* entrees: 	
   	- liste1
	- liste2 
   	sorties:
   	- bool 
	pre:
	- l1.length < l2.length
Teste l'inclusion de l1 dans l2   
*)

(* appartenance var dans liste *)
let varInclude t l =
	List.fold_right (fun vt vq -> (t = vt) || vq) l false;
;;

(* inclusion liste dans une autre *)
let listInclude l1 l2 = 
	List.fold_right (fun vt vq -> (varInclude vt l2) && vq) l1 true;
;;


(* ------------------------ tri_topologique ------------------------ *)
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   
val tri_topologique : DAG.t -> DAG.vertex list
*)

let tri_topologique g =
	let rec tri_rec y z =
		match y with
		| [] -> []
		| t::q ->	
			let zp = t::z in
				let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t q in	
		 		(tri_rec yp zp)@[t]	
	in tri_rec (sansDep g) [] 
;;



(* trace d'execution 
   definie en Section 2 de l'enonce (voir Figure 2) 
*)
type trace = (DAG.vertex list) list;;


(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous n'utiliserez pas d'heuristique
val ordonnanceur_sans_heuristique : int -> DAG.t -> trace
   *)
let rec etape_ordonnanceur g y z res =
	let rec tri_rec y ytodo z result res =
		match y with
		| [] -> (ytodo, z ,result)
		| t::q ->	
			(* not enough resources *)
			if (res < 1) then
				(ytodo@y, z, result)
			else
				(* enough *)
				let zp = t::z in
					let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t ytodo in	
	 				(tri_rec q yp zp (result@[t]) (res - 1))
	in tri_rec y [] z [] res 
;;

let ordonnanceur_sans_heuristique res g =
	let rec ordonnanceur_full y z result =
		match y with
		|[] -> result
		|t::q ->
			let (y_etape, z_etape, result_etape) = etape_ordonnanceur g y z res 
			in ordonnanceur_full y_etape z_etape (result@[result_etape])
	in ordonnanceur_full (sansDep g) [] []
;;


(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous utiliserez une heuristique pour ameliorer la duree de la trace
val ordonnanceur_avec_heuristique : int -> DAG.t -> trace 
   *)
(* heuristique:
	- traiter en priorite les noeuds de plus grand chemin critique
	- on se sert de la mark des vertex pour stocker l'information: taille du chemin critique issu de ce vertex
*)
let rec length_chemin_critique g v =
	let listeSucc = (succ g v) in
		match listeSucc with
		|[] -> 0
		|t::q -> 
			1 + List.fold_left (fun acc vx -> max acc (length_chemin_critique g vx)) 0 listeSucc	
;;

let sortBy_chemin_critique g l =
	List.sort (fun a b -> if ((Mark.get a) > (Mark.get b)) then -1 else (if ((Mark.get a) < (Mark.get b)) then 1 else 0)) l
;;

let markBy_chemin_critique g =
	fold_vertex (fun vt vq -> Mark.set vt (length_chemin_critique g vt)) g ()
;;


let ordonnanceur_avec_heuristique res g =
	begin
		markBy_chemin_critique g;
		let rec ordonnanceur_full y z result =
			match y with
			|[] -> result
			|t::q ->
				let (y_etape, z_etape, result_etape) = etape_ordonnanceur g (sortBy_chemin_critique g y) z res 
				in ordonnanceur_full y_etape z_etape (result@[result_etape])
		in ordonnanceur_full (sansDep g) [] []
	end	
;;


(* entrees: 
   - un nombre entier de ressources r
   - memoire disponible M
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - on suppose une contrainte mémoire (section 4)
   - vous utiliserez la meme heuristique que le cas non-contraint 
val ordonnanceur_contrainte_memoire : int -> int -> DAG.t -> trace
   *)

let rec etape_ordonnanceur_contrainte g y z res mem =
	let rec tri_rec y ytodo z result res mact =
		match y with
		| [] -> (ytodo, z ,result)
		| t::q ->	
			(* not enough resources or memory *)
			if ((res < 1) || (mem < mact)) then
				(ytodo@y, z, result)
			else
				(* enough *)
				let zp = t::z in
					let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t ytodo in	
 					(tri_rec q yp zp (result@[t]) (res - 1) (mact + (DAG.Attributes.vertex_mem t)))	
	in tri_rec y [] z [] res 0
;;


let ordonnanceur_contrainte_memoire res mem g =
	begin
		markBy_chemin_critique g;
		let rec ordonnanceur_full y z result =
			match y with
			|[] -> result
			|t::q ->
				let (y_etape, z_etape, result_etape) = etape_ordonnanceur_contrainte g y z res mem
				in ordonnanceur_full (sortBy_chemin_critique_mem g y_etape) z_etape (result@[result_etape])
		in ordonnanceur_full (sansDep g) [] []	
	end
;;


(* entrees: 
   - un nombre entier de ressources r
   - memoire disponible M
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - on suppose une contrainte mémoire (section 4)
   - vous utiliserez une heuristique specifique au cas contraint
val ordonnanceur_contrainte_memoire_bonus : int -> int -> DAG.t -> trace
   *)
(* heuristique : à chaque sous étape on execute le plus grand nombre de tâches possible en remplissant au max la mémoire *)

let sortBy_chemin_critique_mem g l = 
	let rec tri_iter l =  
		match l with
		|[] -> l
		|t1::t2::q ->
			if ((Mark.get t1) = (Mark.get t2)) then 
				if ((DAG.Attributes.vertex_mem t1) <= (DAG.Attributes.vertex_mem t2)) then 
					t1::t2::q					
				else
					t2::t1::q
			else
				t1::t2::q
	in tri_iter (sortBy_chemin_critique g l) 
;; 


let rec etape_ordonnanceur_contrainte_bonus g y z res mem =
	let rec tri_rec y ytodo z result res mact =
		match y with
		| [] -> (ytodo, z ,result)
		| t::q ->	
			(* not enough resources or memory *)
			if ((res < 1) || (mem < mact)) then
				(ytodo@y, z, result)
			else
				(* enough *)
				let zp = t::z in
					let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t ytodo in	
 					(tri_rec q yp zp (result@[t]) (res - 1) (mact + (DAG.Attributes.vertex_mem t)))	
	in tri_rec y [] z [] res 0
;;


let ordonnanceur_contrainte_memoire_bonus res mem g =
	begin
		markBy_chemin_critique g;
		let rec ordonnanceur_full y z result =
			match y with
			|[] -> result
			|t::q ->
				let (y_etape, z_etape, result_etape) = etape_ordonnanceur_contrainte_bonus g y z res mem
				in ordonnanceur_full (sortBy_chemin_critique g y_etape) z_etape (result@[result_etape])
		in ordonnanceur_full (sansDep g) [] []	
	end
;;





	 
