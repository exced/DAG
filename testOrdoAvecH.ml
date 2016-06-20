(* TEST ordonnanceur_avec_heuristique *)
(* DAG1 *)
let ordodag1r2h = ordonnanceur_avec_heuristique 2 dag1;;
let printordodag1r2h = printListeListeSommets ordodag1r2h;;

let ordodag1r3h = ordonnanceur_avec_heuristique 3 dag1;;
let printordodag1r3h = printListeListeSommets ordodag1r3h;;

let ordodag1r4h = ordonnanceur_avec_heuristique 4 dag1;;
let printordodag1r4h = printListeListeSommets ordodag1r4h;;

(* DAG2 *)
let ordodag2r2h = ordonnanceur_avec_heuristique 2 dag2;;
let printordodag2r2h = printListeListeSommets ordodag2r2h;;

let ordodag2r3h = ordonnanceur_avec_heuristique 3 dag2;;
let printordodag2r3h = printListeListeSommets ordodag2r3h;;

let ordodag2r4h = ordonnanceur_avec_heuristique 4 dag2;;
let printordodag2r4h = printListeListeSommets ordodag2r4h;;

(* DAG3 *)
let ordodag3r2h = ordonnanceur_avec_heuristique 2 dag3;;
let printordodag3r2h = printListeListeSommets ordodag3r2h;;

let ordodag3r3h = ordonnanceur_avec_heuristique 3 dag3;;
let printordodag3r3h = printListeListeSommets ordodag3r3h;;

let ordodag3r4h = ordonnanceur_avec_heuristique 4 dag3;;
let printordodag3r4h = printListeListeSommets ordodag3r4h;;

(* DAG4 *)
let ordodag4r2h = ordonnanceur_avec_heuristique 2 dag4;;
let printordodag4r2h = printListeListeSommets ordodag4r2h;;

let ordodag4r3h = ordonnanceur_avec_heuristique 3 dag4;;
let printordodag4r3h = printListeListeSommets ordodag4r3h;;

let ordodag4r4h = ordonnanceur_avec_heuristique 4 dag4;;
let printordodag4r4h = printListeListeSommets ordodag4r4h;;
