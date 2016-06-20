(* tests ordonnanceur sans heuristique *)

(*DAG1*)
let ordodag1r2 = ordonnanceur_sans_heuristique 2 dag1;;
let printordodag1r2 = printListeListeSommets ordodag1r2;;

let ordodag1r3 = ordonnanceur_sans_heuristique 3 dag1;;
let printordodag1r3 = printListeListeSommets ordodag1r3;;

let ordodag1r4 = ordonnanceur_sans_heuristique 4 dag1;;
let printordodag1r4 = printListeListeSommets ordodag1r4;;

(*DAG2*)
let ordodag2r2 = ordonnanceur_sans_heuristique 2 dag2;;
let printordodag2r2 = printListeListeSommets ordodag2r2;;

let ordodag2r3 = ordonnanceur_sans_heuristique 3 dag2;;
let printordodag2r3 = printListeListeSommets ordodag2r3;;

let ordodag2r4 = ordonnanceur_sans_heuristique 4 dag2;;
let printordodag2r4 = printListeListeSommets ordodag2r4;;

(*DAG3*)
let ordodag3r2 = ordonnanceur_sans_heuristique 2 dag3;;
let printordodag3r2 = printListeListeSommets ordodag3r2;;

let ordodag3r3 = ordonnanceur_sans_heuristique 3 dag3;;
let printordodag3r3 = printListeListeSommets ordodag3r3;;

let ordodag3r4 = ordonnanceur_sans_heuristique 4 dag3;;
let printordodag3r4 = printListeListeSommets ordodag3r4;;

let ordodag3r7 = ordonnanceur_sans_heuristique 7 dag3;;
let printordodag3r7 = printListeListeSommets ordodag3r7;;

(*DAG4*)
let ordodag4r2 = ordonnanceur_sans_heuristique 2 dag4;;
let printordodag4r2 = printListeListeSommets ordodag4r2;;

let ordodag4r3 = ordonnanceur_sans_heuristique 3 dag4;;
let printordodag4r3 = printListeListeSommets ordodag4r3;;

let ordodag4r4 = ordonnanceur_sans_heuristique 4 dag4;;
let printordodag4r4 = printListeListeSommets ordodag4r4;;

let ordodag4r7 = ordonnanceur_sans_heuristique 7 dag4;;
let printordodag4r7 = printListeListeSommets ordodag4r7;;

