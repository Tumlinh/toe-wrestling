open Graph

type path = string

(* Pointers (gross but it'd be unpractical to proceed otherwise) *)
let rem_matches_target = ref 0 ;;
let seed = ref 0 ;;



(* Format of files: lines of the form 
 *
 *  p player\tscore
 *  m player1\tplayer2\tnb_matches_remaining
 *
 *)



(* Read players *)
let read_player line players =
	try
		(* Fill players hash table (player:string -> score:int) *)
		Scanf.sscanf line "p %s@\t%s" (fun player score -> Hashtbl.add players player (int_of_string score))
	with e -> Printf.printf "Cannot read player in line - %s:\n%s\n" (Printexc.to_string e) line



(* Read remaining matches *)
let read_matches line rem_matches targeted_player =
	try
		let (player1, player2, nb_matches) = Scanf.sscanf line "m %s@\t%s@\t%s" (fun player1 player2 nb_matches -> (player1, player2, nb_matches)) in
		if player1 <> targeted_player && player2 <> targeted_player then
			(* Fill rem_matches hash table ((player1:string, player2:string) -> nb_matches:int) *)
			Hashtbl.add rem_matches (player1, player2) (int_of_string nb_matches)
		else
			(* Update the number of remaining matches of the targeted player by reference *)
			rem_matches_target := !rem_matches_target + (int_of_string nb_matches)
	with e -> Printf.printf "Cannot read match in line - %s:\n%s\n" (Printexc.to_string e) line



(* Build graph from file *)
let from_file path targeted_player =
	(* Fill hash tables from file content *)
	let infile = open_in path
	and players = Hashtbl.create 60
	and rem_matches = Hashtbl.create 60
	in
	let rec loop () =
		try
			let line = input_line infile in
			let () =
				if line = "" then ()
				else match line.[0] with
					| 'p' -> read_player line players
					| 'm' -> read_matches line rem_matches targeted_player
					| _ -> ()
			in
			loop ()
		with End_of_file -> ()
	in
	loop () ;
	close_in infile ;
	
	(* Generate graph from hash tables *)
	let graph = new_graph ()
	and source_id = "0"
	and sink_id = "1" in
	add_vertex graph "s" source_id ;
	add_vertex graph "t" sink_id ;
	Hashtbl.iter (fun player score ->
		if player <> targeted_player then
			begin
				(* Add player node *)
				add_vertex graph player player ;
				(* Add constraint on the player's score *)
				let score_target = Hashtbl.find players targeted_player in
				let max_winnable = !rem_matches_target + score_target - score in
				add_edge graph player sink_id (string_of_int max_winnable)
			end
	) players ;
	Hashtbl.iter (fun (player1, player2) nb_matches ->
		if player1 <> targeted_player && player2 <> targeted_player then
			begin
				let players = player1 ^ "\n" ^ player2 in
				(* Add match *)
				add_vertex graph players players ;
				(* Add number of remaining matches *)
				add_edge graph source_id players (string_of_int nb_matches) ;
				(* Add link between match and players involved (infinite capacity) *)
				add_edge graph players player1 "999" ;
				add_edge graph players player2 "999"
			end
	) rem_matches ;
	(graph, source_id, sink_id)



(* Return random quotes *)
let rand_bullshit_engine possible =
	let file = if possible then "quotes_possible.txt" else "quotes_impossible.txt" in
	(* Get number of lines *)
	let infile = open_in file in
	let rec loop counter =
		try
			let _ = input_line infile in
			loop (counter + 1)
		with End_of_file -> counter
	in
	let nb_lines = loop 0 in
	close_in infile ;
	
	(* Generate random number between 0 and nb_lines *)
	(* Apparently Random.self_init is not working *)
	let infile = open_in "/dev/urandom" in
	let seed_bytes = Bytes.create 8 in
	ignore (input infile seed_bytes 0 8) ;
	close_in infile ;
	(* Convert bytes to int *)
	Bytes.iter (fun byte -> seed := (!seed lsl 8) + Char.code byte) seed_bytes ;
	(*Printf.printf "<<<%d\n" !seed ;*)
	Random.init !seed ;
	let rand = Random.int nb_lines in
	
	(* Get the line associated with the random number *)
	let infile = open_in file in
	let rec loop counter =
		try
			let line = input_line infile in
			if counter = rand then
				line
			else
				loop (counter + 1)
		with End_of_file -> ""
	in
	let rand_line = loop 0 in
	close_in infile ;
	rand_line



(* Main *)
let () =
	(* Read CLI arguments *)
	if Array.length Sys.argv <> 4 then
		begin
			Printf.printf "Usage: %s targeted_player infile outfile\n%!" Sys.argv.(0) ;
			exit 1
		end ;
	let targeted_player = Sys.argv.(1)
	and infile = Sys.argv.(2)
	and outfile = Sys.argv.(3) in
	
	(* Read and cache graph *)
	let (graph, source_id, sink_id) = from_file infile targeted_player in
	(*let graph_cached = cache_graph graph source_id "" in*)
	
	(* Facility for centralising arguments *)
	(* Enjoy that genericity fella! *)
	let edge_ref = "0"
	and conv1_func = int_of_string
	and conv2_func = string_of_int
	and print_unknown = Printf.printf "%d\n%!"
	and comp_func = (>)
	and diff_func = (-) in
		
	(* Ford Fulkerson *)
	Ff.ff graph source_id sink_id edge_ref conv1_func conv2_func print_unknown comp_func diff_func ;
	
	(* Check whether the targeted player can win or not (i.e. every matches have been processed or not respectively) *)
	let vi_source = find_vertex graph source_id in
	let source_edges = vi_source.outedges in
	let matches_not_processed =
		let rec iter_edges = function
		| [] -> false
		| (edge, _) :: rest ->
			(* If an edge from the source is not positive, return true *)
			if not (comp_func (conv1_func edge) (conv1_func edge_ref)) then
				iter_edges rest
			else
				true
		in
		iter_edges source_edges
	in
	
	(* Display results *)
	let msg = (rand_bullshit_engine (not matches_not_processed) ^ "\n") in
	try
		let fmt = Scanf.format_from_string msg "%s" in
		Printf.printf fmt targeted_player
	with e -> print_string msg ;
	
	(* Familial export *)
	Gfile.export outfile graph source_id sink_id ; 
	
	exit 0