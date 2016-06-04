open Graph.Pack.Digraph;;

(* exemples de graphes *) 
(* le graphe de la page 5 du sujet de projet *)
let g1 = create ();;
let a=V.create 0;;
let b=V.create 1;;
let c=V.create 2;;
let d=V.create 3;;
let e=V.create 4;;
let f=V.create 5;;
add_vertex g1 a;;
add_vertex g1 b;;
add_vertex g1 c;;
add_vertex g1 d;;
add_vertex g1 e;;
add_vertex g1 f;;
add_edge g1 a d;;
add_edge g1 b d;;
add_edge g1 b e;;
add_edge g1 c e;;
add_edge g1 d f;;
add_edge g1 e f;;

let g2 = create ();;
let a=V.create 0;;
let b=V.create 1;;
let c=V.create 2;;
let d=V.create 3;;
let e=V.create 4;;
let f=V.create 5;;
let g=V.create 6;;
add_vertex g2 a;;
add_vertex g2 b;;
add_vertex g2 c;;
add_vertex g2 d;;
add_vertex g2 e;;
add_vertex g2 f;;
add_vertex g2 g;;
add_edge g2 a d;;
add_edge g2 b e;;
add_edge g2 c f;;
add_edge g2 d e;;
add_edge g2 e f;;
add_edge g2 f g;;


(* ------------------------ Affichage ------------------------ *)
let printSommets g = fold_vertex (fun v qt -> Format.printf "%i " (V.label v)) g ();;
let printAretes g = fold_edges (fun v1 v2 qt-> Format.printf "- %i %i -" (V.label v1) (V.label v2)) g ();;
let printListeSommets l = List.fold_right (fun v qt -> Format.printf "%i " (V.label v)) l ();;
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
let l1 = 1::2::3::[];;
let l2 = 2::3::[];;
let l3 = 4::2::3::[];;
let l4 = 1::2::5::6::[];;
let l5 = 7::2::5::4::[];;
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
let minMark l =
	List.fold_right (fun vt vq -> if ((Mark.get vt) < (Mark.get vq)) then vt else vq) (List.tl l) (List.hd l)
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
	let rec tri_rec y z m =
		match y with
		| [] -> []
		| t::q ->	
		begin
			Mark.set t (m+1);
			let zp = t::z in
				let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t q in	
		 		(tri_rec yp zp (m+1))@[t]
		end	
	in tri_rec (sansDep g) [] 0
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
	let rec tri_rec y ytodo z result res m =
		match y with
		| [] -> (ytodo, z ,result)
		| t::q ->	
			(* not enough resources *)
			if (res < 1) then
				(y@ytodo, z, result)
			else
				begin
					(* enough *)
					Mark.set t (m+1);
					let zp = t::z in
						let yp = fold_succ (fun vt vq -> if (listInclude (pred g vt) zp) then vq@[vt] else vq) g t ytodo in	
	 					(tri_rec q yp zp (result@[t]) (res - 1) (m+1))
				end	
	in tri_rec y [] z [] res 0
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
	traiter en priorite les noeuds de plus grand chemin critique
*)
let rec length_chemin_critique g v =
	let listeSucc = (succ g v) in
		match listeSucc with
		|[] -> 0
		|t::q -> 
			1 + List.fold_left (fun acc vx -> max acc (length_chemin_critique g vx)) 0 listeSucc	
;;


(* tests *)
let lengthg1a = length_chemin_critique g1 (find_vertex g1 0);;
let lengthg2a = length_chemin_critique g2 (find_vertex g2 0);;
let lengthg2b = length_chemin_critique g2 (find_vertex g2 1);;
let lengthg2c = length_chemin_critique g2 (find_vertex g2 2);;

let sortBy_chemin_critique g l =
	List.sort (fun a b -> if ((length_chemin_critique g a)  < (length_chemin_critique g b)) then -1 else (if ((length_chemin_critique g a) > (length_chemin_critique g b)) then 1 else 0)) l
;;

(* tests *)
let sortedg2= sortBy_chemin_critique g2 [(find_vertex g2 0);(find_vertex g2 1);(find_vertex g2 2)];;
let printBeforeSort = printListeSommets [(find_vertex g2 0);(find_vertex g2 1);(find_vertex g2 2)];;
let printAfterSort = printListeSommets sortedg2;;

let ordonnanceur_avec_heuristique res g =
	let rec ordonnanceur_full y z result =
		match y with
		|[] -> result
		|t::q ->
			let (y_etape, z_etape, result_etape) = etape_ordonnanceur g y z res 
			in ordonnanceur_full (sortBy_chemin_critique g y_etape) z_etape (result@[result_etape])
	in ordonnanceur_full (sansDep g) [] []	
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





	 
