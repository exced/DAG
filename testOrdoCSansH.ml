(* TEST ordonnanceur_avec_contrainte *)

(*DAG1*)
let ordodag1r2m1hc = ordonnanceur_contrainte_memoire 2 1 dag1;;
let printordodag1r2m1hc = printListeListeSommets ordodag1r2m1hc;;

let ordodag1r2m2hc = ordonnanceur_contrainte_memoire 2 2 dag1;;
let printordodag1r2m2hc = printListeListeSommets ordodag1r2m2hc;;

let ordodag1r2m3hc = ordonnanceur_contrainte_memoire 2 3 dag1;;
let printordodag1r2m3hc = printListeListeSommets ordodag1r2m3hc;;

(*DAG2*)
let ordodag2r2m1hc = ordonnanceur_contrainte_memoire 2 1 dag2;;
let printordodag1r2m1hc = printListeListeSommets ordodag2r2m1hc;;

let ordodag2r2m2hc = ordonnanceur_contrainte_memoire 2 2 dag2;;
let printordodag1r2m2hc = printListeListeSommets ordodag2r2m2hc;;

let ordodag2r2m3hc = ordonnanceur_contrainte_memoire 2 3 dag2;;
let printordodag1r2m3hc = printListeListeSommets ordodag2r2m3hc;;

(*DAG3*)
let ordodag3r2m1hc = ordonnanceur_contrainte_memoire 2 1 dag3;;
let printordodag3r2m1hc = printListeListeSommets ordodag3r2m1hc;;

let ordodag3r2m2hc = ordonnanceur_contrainte_memoire 2 2 dag3;;
let printordodag3r2m2hc = printListeListeSommets ordodag3r2m2hc;;

let ordodag3r2m3hc = ordonnanceur_contrainte_memoire 2 3 dag3;;
let printordodag3r2m3hc = printListeListeSommets ordodag3r2m3hc;;

(*DAG4*)
let ordodag4r2m1hc = ordonnanceur_contrainte_memoire 2 1 dag4;;
let printordodag4r2m1hc = printListeListeSommets ordodag4r2m1hc;;

let ordodag4r2m2hc = ordonnanceur_contrainte_memoire 2 2 dag4;;
let printordodag4r2m2hc = printListeListeSommets ordodag4r2m2hc;;

let ordodag4r2m3hc = ordonnanceur_contrainte_memoire 2 3 dag4;;
let printordodag4r2m3hc = printListeListeSommets ordodag4r2m3hc;;