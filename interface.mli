
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
val tri_topologique : DAG.t -> DAG.vertex list

(* trace d'execution 
   definie en Section 2 de l'enonce (voir Figure 2)
*)
type trace = (DAG.vertex list) list 

(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous n'utiliserez pas d'heuristique
   *)
val ordonnanceur_sans_heuristique : int -> DAG.t -> trace

(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous utiliserez une heuristique pour ameliorer la duree de la trace 
   *)
val ordonnanceur_avec_heuristique : int -> DAG.t -> trace

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
   *)
val ordonnanceur_contrainte_memoire : int -> int -> DAG.t -> trace

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
   *)
val ordonnanceur_contrainte_memoire_bonus : int -> int -> DAG.t -> trace

