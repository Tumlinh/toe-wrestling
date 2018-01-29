open Graph

let rec print_path = function
	| [] -> print_string "\n"
	| x :: rest -> Printf.fprintf stdout "%s - %!" x ; print_path rest



let debug_info graph path node_id =
	let nb_children node_id =
		let vi_node = find_vertex graph node_id in
		List.length vi_node.outedges
	in
	Printf.fprintf stdout "node_id:%s  nb_children:%d\nPath: %!" node_id (nb_children node_id) ;
	print_path path



(* TODO: handle reverse edges *)
(* Ford Fulkerson algorithm wrapper (optimises recursion) *)
let ff graph source_id sink_id edge_ref conv1_func conv2_func print_unknown comp_func diff_func =
	let rec ff_sub node_id path =
	
		let rec upd_path graph path =
			(* Find min edge within the path *)
			let rec find_min_edge path min = match path with
				| [] -> min
				| node_id :: [] -> min
				| node1_id :: node2_id :: rest ->
					let elabel = find_edge graph node2_id node1_id in	(* /!\ path is reversed *)
					if comp_func min (conv1_func elabel) then
						find_min_edge (node2_id :: rest) (conv1_func elabel)
					else
						find_min_edge (node2_id :: rest) min
			in
			(* Update cost of edges *)
			let rec upd_edges path edge_val = match path with
				| [] -> ()
				| node_id :: [] -> ()
				| node1_id :: node2_id :: rest ->
					(* Get edge *)
					let elabel = find_edge graph node2_id node1_id in
					(* Replace edge with the new one *)
					let new_elabel = conv2_func (diff_func (conv1_func elabel) (edge_val)) in
					add_edge graph node2_id node1_id new_elabel ;
					(*print_string "new e: " ;
					print_unknown (conv1_func new_elabel) ;*)
					upd_edges (node2_id :: rest) edge_val
			in
			(* Initialise 'min' with the last edge of the path *)
			let min_init = match path with
				| node1_id :: node2_id :: _ -> find_edge graph node2_id node1_id
				| _ -> failwith "Path too short"
			in
			let min = find_min_edge path (conv1_func min_init) in
			upd_edges path min
		in
		
		(* FF core *)
		match node_id with
			| node_id when node_id = sink_id ->
				(* Path found *)
				(*Printf.fprintf stdout "<<<GOTCHA!!! " ;
				print_path path ;*)
				upd_path graph path
			| _ ->
				(* For each child of the current node *)
				let rec iter_children = function
					| [] -> ()
					| (edge, child_id) :: rest ->
						(*debug_info graph path child_id ;*)
						(* Conditions are met for going DEEPAAA *)
						if comp_func (conv1_func edge) (conv1_func edge_ref) && not (List.exists (fun id -> id = child_id) path) then
							ff_sub child_id (child_id :: path) ;
						(* If a null or negative edge belongs to the current path, there is no point in resuming the DFS *)
						(* This optimisation is valid for significant graphs *)
						let rec contains_null = function
							| [] -> false
							| node_id :: [] -> false
							| node1_id :: node2_id :: rest ->
								(* Get edge *)
								let elabel = find_edge graph node2_id node1_id in
								(* Compare edge value to reference *)
								if comp_func (conv1_func elabel) (conv1_func edge_ref) then
									contains_null rest
								else
									true
						in
						if not (contains_null path) then
							(* Resume DFS *)
							iter_children rest
				in
				let vi_node = find_vertex graph node_id in
				iter_children vi_node.outedges
	in
	ff_sub source_id [source_id]



(* Compute max flow *)
let max_flow graph_init graph_processed source_id edge_ref conv1_func diff_func add_func =
	let vi_source = find_vertex graph_init source_id in
	let source_edges = vi_source.outedges in
	(* For each edge going out of the source, compute its effective flow then sum every contribution *)
	let rec iter_edges = function
		| [] -> conv1_func edge_ref
		| (edge, node_id) :: rest ->
			let edge_init = conv1_func edge in
			let edge_processed = conv1_func (find_edge graph_processed source_id node_id) in
			add_func (iter_edges rest) (diff_func edge_init edge_processed)
	in
	iter_edges source_edges