open Graph.Pack.Digraph;;
open DAG;;

let g16 = create ();;
let ag16=V.create ("a",2);;
let bg16=V.create ("b",2);;
let cg16=V.create ("c",2);;
let dg16=V.create ("d",1);;
let eg16=V.create ("e",1);;
let fg16=V.create ("f",1);;
let gg16=V.create ("g",2);;
let hg16=V.create ("h",2);;

add_vertex g16 ag16;;
add_vertex g16 bg16;;
add_vertex g16 cg16;;
add_vertex g16 dg16;;
add_vertex g16 eg16;;
add_vertex g16 fg16;;
add_vertex g16 gg16;;
add_vertex g16 hg16;;

add_edge g16 ag16 bg16;;
add_edge g16 ag16 cg16;;
add_edge g16 cg16 dg16;;
add_edge g16 cg16 eg16;;
add_edge g16 cg16 fg16;;
add_edge g16 bg16 gg16;;
add_edge g16 gg16 hg16;;


let g16r = ordonnanceur_contrainte_memoire 3 3 g16;;
let printg16 = printListeListeSommets g16r;;


