open Core
open Sims

let transpose m =
  let n = Array.length m in
  Array.init n ~f:(fun i ->
    Array.init n ~f:(fun j -> m.(j).(i))
  )

let combine m1 m2 =
  Array.map2_exn m1 m2 ~f:(fun row1 row2 ->
    Array.map2_exn row1 row2 ~f:(fun v1 v2 -> v1 || v2)
  )

let two_sided g = 
  combine g (transpose g)

let _chain sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> j = i + 1)
  )

let _star sz =
  Array.init sz ~f:(fun i ->
    if i = sz - 1 then
      Array.init sz ~f:(fun j -> j <> (sz - 1))
    else
      Array.init sz ~f:(fun _ -> false)
  )

let _complete sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> j > i)
  )

let _wheels sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> if i = sz - 1 then j = 0 else j = i + 1)
  )

let _empty sz = 
  Array.make_matrix ~dimx:sz ~dimy:sz false


let rec add_random_edges g edges_added total_edges sz ~single =
  if edges_added >= total_edges then g
  else
    let src = Random.int sz in
    let dst = Random.int sz in
    if src = dst then add_random_edges g edges_added total_edges sz ~single
    else
      if g.(src).(dst) then
        add_random_edges g edges_added total_edges sz ~single
      else (
        Game.contribute g src dst;
        if not single then Game.contribute g dst src;
        add_random_edges g (edges_added + 1) total_edges sz ~single
      )

(* Create [n_c] cliques of size [c_sz] and graph of size [sz] 
  then sparsely connect these cliques (aka add alpha*n) random edges
  *)
let _common_bilateral ~sz ~c_sz ~n_c ~alpha = 
  let init = _empty sz in
  let create_clique g fst =
    let all = List.init c_sz ~f:(fun i -> fst + i) in
    List.iter all ~f:(fun i ->
      List.iter all ~f:(fun j ->
        if i <> j then Game.contribute g i j
      )
    );
    g
  in
  let with_cliques = 
    List.foldi (List.init n_c ~f:Fun.id) ~init ~f:(fun i acc _ ->
      create_clique acc (i * c_sz)
    )
  in
  let total_edges = sz * alpha in
  add_random_edges with_cliques 0 total_edges sz



  (* TODO maye false *)
let full_cliques ~sz ~c_sz =
  let init = _empty sz in
  let rec aux g current =
    if current >= sz then g
    else
      let all = List.init c_sz ~f:(fun i -> current + i) in
      List.iter all ~f:(fun i ->
        List.iter all ~f:(fun j ->
          if i <> j then Game.contribute g i j
        )
      );
      aux g (current + c_sz)
  in
  aux init 0



let sparsely_connected_cliques ~sz ~c_sz ~alpha =
  let g = full_cliques ~sz ~c_sz in
  let total_edges = sz * alpha in
  add_random_edges g 0 total_edges sz ~single:false


let some_stars ~sz ~star_sz =
  let init = _empty sz in
  let rec aux g current =
    if current >= sz then g
    else
      let all = List.init star_sz ~f:(fun i -> current + i) in
      List.iter all ~f:(fun j ->
       if current <> j then Game.contribute g current j
      );
      aux g (current + star_sz)
  in
  aux init 0


let connected_stars ~sz ~star_sz ~alpha =
  let g = some_stars ~sz ~star_sz in
  let total_edges = sz * alpha in
  add_random_edges g 0 total_edges sz ~single:true


let print_graph g ~uni =
  let n = Array.length g in
  (* for i = 0 to n - 1 do
    printf "%d\n" i; 
  done; *)
  for i = 0 to n - 1 do
    for j = (if uni then 0 else i+1) to n - 1 do
      if g.(i).(j) then
        printf "%d %d\n" i j
    done;
  done;


module UUNash = Equilibria.Make (Undirected_unilateral_game)
module UBNash = Equilibria.Make (Undirected_bilateral_game)
module DUNash = Equilibria.Make (Directed_unilateral_game)



let _sim_bilateral g =
  let cost = 0.2 in
  let module G = Undirected_bilateral_game in
  let module N = UBNash in

  let first_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  
  let _other = Simulation.run_undirected_bilateral_sim g ~cost ~runs:10 in
  let () = print_graph ~uni:false g in
  
  let next_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.pairwise_stability g ~cost |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()


let _sim_unilateral g =
  let cost = 0.1 in
  let module G = Undirected_unilateral_game in
  let module N = UUNash in
  let first_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let _other = Simulation.run_undirected_unilateral_sim g ~cost ~runs:1000 in
  let () = print_graph ~uni:true g in
  
  let next_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.check_simple_nash g ~cost ~strict:false |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()

let () = UUNash.check_simple_nash (_star 20) ~cost:1.2 ~strict:false |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Unilateral: %b\n"
let () = DUNash.check_simple_nash (_wheels 20) ~cost:25. ~strict:true |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Directed Unilateral: %b\n"
let () = UBNash.pairwise_stability (_star 20 |> two_sided) ~cost:0.8 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n"

let sparse_star = connected_stars ~sz:9 ~star_sz:3 ~alpha:1
let _sparse_clique = sparsely_connected_cliques ~sz:20 ~c_sz:4 ~alpha:1
let () = _sim_unilateral sparse_star
(* let () = _sim_bilateral _sparse_clique *)

(* let () = UBNash.pairwise_stability (_empty 1500) ~cost:0.8 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n" *)
(* let () = UBNash.pairwise_stability (_common_bilateral ~sz:200 ~c_sz:10 ~n_c:5 ~alpha:1) ~cost:1.5 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n" *)
(* let () = UBNash.pairwise_stability sparse_clique ~cost:1.5 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n" *)
(* let _new_graph = Simulation.run_undirected_bilateral_sim (_common_bilateral ~sz:200 ~c_sz:10 ~n_c:5 ~alpha:1) ~cost:1.5 ~runs:100 *)

