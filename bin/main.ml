open Core
open Sims
module UUNash = Equilibria.Make (Undirected_unilateral_game)
module UBNash = Equilibria.Make (Undirected_bilateral_game)
module DUNash = Equilibria.Make (Directed_unilateral_game)


let runs = 1000
let add_new_lines cnt = 
  for _ = 0 to cnt - 1 do
    printf "\n"
  done

let sim_unilateral g ~cost =
  let module G = Undirected_unilateral_game in
  let module N = UUNash in
  let first_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = Graph_gen.print_graph ~uni:true g in
  add_new_lines 5;

  let () = Simulation.run_undirected_unilateral_sim g ~cost ~runs in
  let () = Graph_gen.print_graph ~uni:true g in
  
  let next_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.check_simple_nash g ~cost ~strict:false |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()

let sim_bilateral g ~cost =
  let module G = Undirected_bilateral_game in
  let module N = UBNash in

  let first_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = Graph_gen.print_graph ~uni:false g in
  add_new_lines 5;
  let () = Simulation.run_undirected_bilateral_sim g ~cost ~runs in
  let () = Graph_gen.print_graph ~uni:false g in
  let next_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.pairwise_stability g ~cost |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()



let cool_stars = Graph_gen.connected_stars ~sz:9 ~star_sz:3 ~total_edges:4
let cool_cliques = Graph_gen.sparsely_connected_cliques ~sz:20 ~c_sz:5 ~alpha:1

let comp = Graph_gen.complete 3
let str = Graph_gen.star 3

let() = sim_bilateral cool_cliques ~cost:2.
let () = sim_unilateral cool_stars ~cost:1.5



let () = Graph_gen.print_graph comp ~uni:true
let () = add_new_lines 4
let () = Graph_gen.print_graph str ~uni:true

let () = printf "\n%f\n" (UUNash.all_player_compute ~cost:10. ~game:comp ~network:(Network.build comp ~f:Undirected_unilateral_game.edge_formation))
let () = printf "\n%f\n" (UUNash.all_player_compute ~cost:10. ~game:str ~network:(Network.build str ~f:Undirected_unilateral_game.edge_formation))