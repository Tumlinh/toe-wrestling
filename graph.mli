
(* Type of a directed graph in which vertices have labels of type 'v
 * and edges have labels of type 'e. *)
type ('v, 'e) graph

(* A graph is a mutable structure. *)

(* Each vertex has a unique identifier (a name). *)
type id = string

(* Information about vertices. *)
type ('v, 'e) vertex_info =
  { (* Each vertex has a label. *)
    label: 'v ;

    (* A unique identifier. *)
    id: id ;

    (* Outgoing edges : a list of edge label & destination vertex. *)
    outedges: ('e * id) list ;

    (* Ingoing edges : a list of edge label & origin vertex. 
     * The list is empty if the back option is false (see new_graph). *)
    inedges: ('e * id) list }


(* Find a vertex given an id.
 * @raise Failure if the id is unknown. *)
val find_vertex: ('v, 'e) graph -> id -> ('v, 'e) vertex_info

(* Find an edge given source and destination. *)
val find_edge: ('v, 'e) graph -> id -> id -> 'e

(**************  CONSTRUCTORS  **************)

(* Creates a new (empty) graph. *)
val new_graph: unit -> ('v, 'e) graph

(* Add a vertex to the graph.
 * If a vertex already exists with this identifier, its label is replaced by this one. *)
val add_vertex: ('v, 'e) graph -> 'v -> id -> unit

(* Add an edge given the ids of origin (id1) and destination (id2).
 * If an edge already exists between id1 and id2, its label is replaced by this one. *)
val add_edge: ('v, 'e) graph -> id -> id -> 'e -> unit


(**************  COMBINATORS, ITERATORS  **************)

(* Iterate on all vertices. *)
val v_iter: ('v, 'e) graph -> (('v,'e) vertex_info -> unit) -> unit

(* map graph vmap emap: maps all vertex labels by vmap and all edges label by emap
 * Returns a new graph. Vertices keep the same identifiers.
 * Beware that emap is called twice on each edge (ingoing and outgoing). *)

val map: ('v1, 'e1) graph -> ('v1 -> 'v2) -> ('e1 -> 'e2) -> ('v2, 'e2) graph

(* Cache interesting parts of a graph for computing max flow eventually without using too much memory *)
val cache_graph: ('v, 'e) graph -> id -> 'v -> ('v, 'e) graph