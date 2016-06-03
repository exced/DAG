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

(* fonctions d'affichage *)
let printSommets g = fold_vertex (fun v qt -> Format.printf "%i " (V.label v)) g ();;
let printAretes g = fold_edges (fun v1 v2 qt-> Format.printf "- %i %i -" (V.label v1) (V.label v2)) g ();;


(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   
val tri_topologique : DAG.t -> DAG.vertex list
*)

	 
