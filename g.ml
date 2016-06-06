open Graph.Pack.Digraph;;
open DAG;;

(* exemples de graphes *) 
(* le graphe de la page 5 du sujet de projet *)
let g1 = create ();;
let ag1=V.create ("a",2);;
let bg1=V.create ("b",1);;
let cg1=V.create ("c",1);;
let dg1=V.create ("d",3);;
let eg1=V.create ("e",2);;
let fg1=V.create ("f",1);;
add_vertex g1 ag1;;
add_vertex g1 bg1;;
add_vertex g1 cg1;;
add_vertex g1 dg1;;
add_vertex g1 eg1;;
add_vertex g1 fg1;;
add_edge g1 ag1 dg1;;
add_edge g1 bg1 dg1;;
add_edge g1 bg1 eg1;;
add_edge g1 cg1 eg1;;
add_edge g1 dg1 fg1;;
add_edge g1 eg1 fg1;;



let g2 = create ();;
let ag2=V.create ("a",0);;
let bg2=V.create ("b",1);;
let cg2=V.create ("c",2);;
let dg2=V.create ("d",3);;
let eg2=V.create ("e",4);;
let fg2=V.create ("f",5);;
let gg2=V.create ("g",6);;
add_vertex g2 ag2;;
add_vertex g2 bg2;;
add_vertex g2 cg2;;
add_vertex g2 dg2;;
add_vertex g2 eg2;;
add_vertex g2 fg2;;
add_vertex g2 gg2;;
add_edge g2 ag2 dg2;;
add_edge g2 bg2 eg2;;
add_edge g2 cg2 fg2;;
add_edge g2 dg2 eg2;;
add_edge g2 eg2 fg2;;
add_edge g2 fg2 gg2;;



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

(* tests *)
let testSansDepg1 = sansDep g1;;
let ltestSansDepg1 = printListeSommets testSansDepg1;; 

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

(* prise en main de la fonction List.sort *) 
let sortByUe l =
	List.sort (fun a b -> if a < b then -1 else (if a > b then 1 else 0)) l
;;

(* tests *)
let l1 = [1;2;3];;
let l2 = [2;3];;
let l3 = [4;2;3];;
let l4 = [1;2;5;6];;
let l5 = [7;2;5;4];;
varInclude 1 l1;;
listInclude l2 l1;;
listInclude l3 l4;;
let lsorted = sortByUe l5;;

(* ------------------------ Manipulation Graphe ------------------------ *)
(* entrees: 	
   	- vertex
	- g
   	sorties:
   	- bool 
	pre:   
*)

let sortByMarkUe l =
	List.sort (fun a b -> if (Mark.get a) > (Mark.get b) then -1 else (if (Mark.get a) < (Mark.get b) then 1 else 0)) l
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

(* tests *)
let trig1 = tri_topologique g1;;
let printtrig1 = printListeSommets trig1;;



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
				(y@ytodo, z, result)
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

(* tests *)
let ordog1 = ordonnanceur_sans_heuristique 3 g1;;
let printordog1 = printListeListeSommets ordog1;;
let ordog2 = ordonnanceur_sans_heuristique 3 g2;;
let printordog2 = printListeListeSommets ordog2;;

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

(* tests *)
let lengthg1a = length_chemin_critique g1 ag1;;
let lengthg2a = length_chemin_critique g2 ag2;;
let lengthg2b = length_chemin_critique g2 bg2;;
let lengthg2c = length_chemin_critique g2 cg2;;


let sortBy_chemin_critique g l =
	List.sort (fun a b -> if ((Mark.get a)  < (Mark.get b)) then -1 else (if ((Mark.get a) > (Mark.get b)) then 1 else 0)) l
;;

let markBy_chemin_critique g =
	fold_vertex (fun vt vq -> Mark.set vt (length_chemin_critique g vt)) g ()
;;

(* tests *)
let test_chemin_critique_mark = begin (markBy_chemin_critique g1); printSommetsMark g1 end ;;

(* tests *)
let sortedg2= sortBy_chemin_critique g2 [ag2;bg2;cg2];;
let printBeforeSort = printListeSommets [ag2;bg2;cg2];;
let printAfterSort = printListeSommets sortedg2;;

let ordonnanceur_avec_heuristique res g =
	begin
		markBy_chemin_critique g;
		let rec ordonnanceur_full y z result =
			match y with
			|[] -> result
			|t::q ->
				let (y_etape, z_etape, result_etape) = etape_ordonnanceur g y z res 
				in ordonnanceur_full (sortBy_chemin_critique g y_etape) z_etape (result@[result_etape])
		in ordonnanceur_full (sansDep g) [] []
	end	
;;

(* tests *)
let ordog1h = ordonnanceur_avec_heuristique 3 g1;;
let printordog1h = printListeListeSommets ordog1h;;
let ordog2h = ordonnanceur_avec_heuristique 3 g2;;
let printordog2h = printListeListeSommets ordog2h;;


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
				(y@ytodo, z, result)
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
				in ordonnanceur_full (sortBy_chemin_critique g y_etape) z_etape (result@[result_etape])
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







	 
