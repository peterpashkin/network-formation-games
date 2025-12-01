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

let _sim_unilateral g =
  let cost = 0.1 in
  let module G = Undirected_unilateral_game in
  let module N = UUNash in
  let first_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = Graph_gen.print_graph ~uni:true g in
  add_new_lines 5;

  let () = Simulation.run_undirected_unilateral_sim g ~cost ~runs in
  let () = Graph_gen.print_graph ~uni:true g in
  
  let next_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.check_simple_nash g ~cost ~strict:false |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()

let _sim_bilateral g =
  let cost = 0.2 in
  let module G = Undirected_bilateral_game in
  let module N = UBNash in

  let first_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = Graph_gen.print_graph ~uni:false g in
  add_new_lines 5;
  let () = Simulation.run_undirected_bilateral_sim g ~cost ~runs in
  let () = Graph_gen.print_graph ~uni:false g in
  let next_cost = N.all_player_compute ~cost ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.pairwise_stability g ~cost |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()
