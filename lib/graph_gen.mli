val two_sided : bool array array -> bool array array
val chain : int -> bool array array
val star : int -> bool array array
val complete : int -> bool array array
val wheels : int -> bool array array
val empty : int -> bool array array
(* can say how many cliques you want *)
val common_bilateral : sz:int ->
    c_sz:int ->
    n_c:int ->
    alpha:int ->
    single:bool ->
    bool array array
(* generates maximal possible cliques and connects them randomly *)
val sparsely_connected_cliques : sz:int -> c_sz:int -> alpha:int -> bool array array
val connected_stars : sz:int -> star_sz:int -> total_edges:int -> bool array array
val print_graph : bool array array -> uni:bool -> unit
