open Graph.Pack.Digraph;;
(* module de test de toutes les fonctions *)
open G;;

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

let l = sansDep g1;;
