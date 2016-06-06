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

let g3 = create ();;
let ag3=V.create ("a",1);;
let bg3=V.create ("b",1);;
let cg3=V.create ("c",1);;
let dg3=V.create ("d",1);;
let eg3=V.create ("e",1);;
let fg3=V.create ("f",1);;
let gg3=V.create ("g",1);;
let hg3=V.create ("h",1);;
let ig3=V.create ("i",1);;
let jg3=V.create ("j",1);;
let kg3=V.create ("k",1);;
let lg3=V.create ("l",1);;
let mg3=V.create ("m",1);;
let ng3=V.create ("n",1);;
let og3=V.create ("o",1);;
add_vertex g3 ag3;;
add_vertex g3 bg3;;
add_vertex g3 cg3;;
add_vertex g3 dg3;;
add_vertex g3 eg3;;
add_vertex g3 fg3;;
add_vertex g3 gg3;;
add_vertex g3 hg3;;
add_vertex g3 ig3;;
add_vertex g3 jg3;;
add_vertex g3 kg3;;
add_vertex g3 lg3;;
add_vertex g3 mg3;;
add_vertex g3 ng3;;
add_vertex g3 og3;;
add_edge g3 ag3 bg3;;
add_edge g3 ag3 cg3;;
add_edge g3 bg3 dg3;;
add_edge g3 bg3 eg3;;
add_edge g3 cg3 fg3;;
add_edge g3 cg3 gg3;;
add_edge g3 dg3 hg3;;
add_edge g3 dg3 ig3;;
add_edge g3 eg3 jg3;;
add_edge g3 eg3 kg3;;
add_edge g3 fg3 lg3;;
add_edge g3 fg3 mg3;;
add_edge g3 gg3 ng3;;
add_edge g3 gg3 og3;;

(* TEST sansDep *)
let testSansDepg1 = sansDep g1;;
let ltestSansDepg1 = printListeSommets testSansDepg1;; 


(* TEST listInclude *)
let l1 = [1;2;3];;
let l2 = [2;3];;
let l3 = [4;2;3];;
let l4 = [1;2;5;6];;
let l5 = [7;2;5;4];;
varInclude 1 l1;;
listInclude l2 l1;;
listInclude l3 l4;;


(* TEST tri_topologique *)
let trig1 = tri_topologique g1;;
let printtrig1 = printListeSommets trig1;;


(* TEST ordonnanceur_sans_heuristique *)
let ordog1 = ordonnanceur_sans_heuristique 3 g1;;
let printordog1 = printListeListeSommets ordog1;;

let ordog2 = ordonnanceur_sans_heuristique 3 g2;;
let printordog2 = printListeListeSommets ordog2;;

let ordog3r1 = ordonnanceur_sans_heuristique 1 g3;;
let printordog3r1 = printListeListeSommets ordog3r1;;

let ordog3r2 = ordonnanceur_sans_heuristique 2 g3;;
let printordog3r2 = printListeListeSommets ordog3r2;;

let ordog3r3 = ordonnanceur_sans_heuristique 3 g3;;
let printordog3r3 = printListeListeSommets ordog3r3;;

let ordog3r4 = ordonnanceur_sans_heuristique 4 g3;;
let printordog3r4 = printListeListeSommets ordog3r4;;

let ordodag2 = ordonnanceur_sans_heuristique 3 dag2;;
let printordodag2 = printListeListeSommets ordodag2;;

let ordodag3r2 = ordonnanceur_sans_heuristique 2 dag3;;
let printordodag3r2 = printListeListeSommets ordodag3r2;;

let ordodag3r3 = ordonnanceur_sans_heuristique 3 dag3;;
let printordodag3r3 = printListeListeSommets ordodag3r3;;

let ordodag3r4 = ordonnanceur_sans_heuristique 4 dag3;;
let printordodag3r4 = printListeListeSommets ordodag3r4;;

let ordodag1r2 = ordonnanceur_sans_heuristique 2 dag1;;
let printordodag1r2 = printListeListeSommets ordodag1r2;;

let ordodag1r3 = ordonnanceur_sans_heuristique 3 dag1;;
let printordodag1r3 = printListeListeSommets ordodag1r3;;

let ordodag1r4 = ordonnanceur_sans_heuristique 4 dag1;;
let printordodag1r4 = printListeListeSommets ordodag1r4;;

(* TEST length_chemin_critique *)
let lengthg1a = length_chemin_critique g1 ag1;;
let lengthg2a = length_chemin_critique g2 ag2;;
let lengthg2b = length_chemin_critique g2 bg2;;
let lengthg2c = length_chemin_critique g2 cg2;;


(* TEST markBy_chemin_critique *)
let test_chemin_critique_mark = begin (markBy_chemin_critique g1); printSommetsMark g1 end ;;

let test_chemin_critique_markdag1 = begin (markBy_chemin_critique dag1); printSommetsMark dag1 end ;;

(* TEST sortBy_chemin_critique *)
let sortedg2= sortBy_chemin_critique g2 [ag2;bg2;cg2];;
let printBeforeSort = printListeSommets [ag2;bg2;cg2];;
let printAfterSort = printListeSommets sortedg2;;


(* TEST ordonnanceur_avec_heuristique *)
let ordog1h = ordonnanceur_avec_heuristique 3 g1;;
let printordog1h = printListeListeSommets ordog1h;;

let ordog2h = ordonnanceur_avec_heuristique 3 g2;;
let printordog2h = printListeListeSommets ordog2h;;


let ordodag3r2h = ordonnanceur_avec_heuristique 2 dag3;;
let printordodag3r2h = printListeListeSommets ordodag3r2h;;

let ordodag3r3h = ordonnanceur_avec_heuristique 3 dag3;;
let printordodag3r3h = printListeListeSommets ordodag3r3h;;

let ordodag3r4h = ordonnanceur_avec_heuristique 4 dag3;;
let printordodag3r4h = printListeListeSommets ordodag3r4h;;

let ordodag1r2h = ordonnanceur_avec_heuristique 2 dag1;;
let printordodag1r2h = printListeListeSommets ordodag1r2h;;

let ordodag1r3h = ordonnanceur_avec_heuristique 3 dag1;;
let printordodag1r3h = printListeListeSommets ordodag1r3h;;

let ordodag1r4h = ordonnanceur_avec_heuristique 4 dag1;;
let printordodag1r4h = printListeListeSommets ordodag1r4h;;


(* TEST ordonnanceur_avec_contrainte *)




