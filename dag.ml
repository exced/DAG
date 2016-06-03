(*
Cree les graphes de test dag1 ... dag5 et genere les fichiers .dot associes.
Pour visualiser les graphes : 
	dot dag.dot -Tps -o dag.ps
	evince dag.ps
*)

module Vertex = struct
  type t = string*int
  let compare : t -> t -> int = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = (0,0)
  let name (nod : string*int) = fst nod
  let memory (nod : string*int) = snd nod
  let strname (nod : string*int) = name nod
  let strmemory (nod : string*int) = string_of_int (memory nod)
end

module Edge = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

module DAG = 
struct
	include Graph.Imperative.Digraph.AbstractLabeled(Vertex)(Edge)
	
	module Display = struct
		include Graph.Imperative.Digraph.AbstractLabeled(Vertex)(Edge)
		let vertex_name v = Vertex.strname (V.label v)
		let graph_attributes _ = []
		let default_vertex_attributes _ = []
		let vertex_attributes v = [`Label ((Vertex.strname (V.label v))^":"^(Vertex.strmemory (V.label v)))]
		let default_edge_attributes _ = []
		let edge_attributes e = []
		let get_subgraph _ = None
	end
	module Dot_ = Graph.Graphviz.Dot(Display)
	module Neato = Graph.Graphviz.Neato(Display)

	let dot_output g f =
		let oc = open_out f in
		if is_directed then Dot_.output_graph oc g else Neato.output_graph oc g;
		close_out oc

end


(*
algorithme de tri topologique utilisant un format de file pour Y
IN
OUT  
*)
let tri_topologique V = 
	let Y = SansDep(V) in
		
;;

