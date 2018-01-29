open Graph

val print_path: id list -> unit
val ff: ('v, 'e) graph -> id -> id -> 'e -> ('e -> 'e2) -> ('e2 -> 'e) -> ('e2 -> unit) -> ('e2 -> 'e2 -> bool) -> ('e2 -> 'e2 -> 'e2) -> unit
val max_flow: ('v, 'e) graph -> ('v, 'e) graph -> id -> 'e -> ('e -> 'e2)-> ('e2 -> 'e2 -> 'e2) -> ('e2 -> 'e2 -> 'e2) -> 'e2