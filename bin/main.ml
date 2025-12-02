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

let sim_bilateral g ~cost ~limit =
  let module G = Undirected_bilateral_game in
  let module N = UBNash in

  let first_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = Graph_gen.print_graph ~uni:false g in
  add_new_lines 5;
  let () = Simulation.run_undirected_bilateral_sim g ~cost ~runs ~limit in
  let () = Graph_gen.print_graph ~uni:false g in
  let next_cost = N.all_player_compute ~cost ~game:g ~network:(Network.build g ~f:G.edge_formation) in
  let () = N.pairwise_stability g ~cost |> (function Action.None -> true | _ -> false) |> printf "Iterated Graph is Nash: %b\n" in
  let () = printf "Initial cost: %f, Final cost: %f\n" first_cost next_cost in
  ()

let () = Random.set_state (Random.State.make [|42|])


let sz = 12
let cool_stars = Graph_gen.connected_stars ~sz ~star_sz:4 ~total_edges:3
let cool_cliques = Graph_gen.sparsely_connected_cliques ~sz ~c_sz:4 ~total_edges:3

let comp = Graph_gen.complete sz
let str = Graph_gen.star sz
let emp = Graph_gen.empty sz

let cost = 0.5


let () = sim_unilateral cool_stars ~cost

let () = printf "\n%f\n" (UUNash.all_player_compute ~cost ~game:comp ~network:(Network.build comp ~f:Undirected_unilateral_game.edge_formation))
let () = printf "\n%f\n" (UUNash.all_player_compute ~cost ~game:str ~network:(Network.build str ~f:Undirected_unilateral_game.edge_formation))
let () = printf "\n%f\n" (UUNash.all_player_compute ~cost ~game:emp ~network:(Network.build emp ~f:Undirected_unilateral_game.edge_formation))

let() = sim_bilateral cool_cliques ~cost ~limit:None

let () = printf "\n%f\n" (UBNash.all_player_compute ~cost ~game:(comp |> Graph_gen.two_sided) ~network:(Network.build (comp |> Graph_gen.two_sided) ~f:Undirected_bilateral_game.edge_formation))
let () = printf "\n%f\n" (UBNash.all_player_compute ~cost ~game:(str |> Graph_gen.two_sided) ~network:(Network.build (str |> Graph_gen.two_sided) ~f:Undirected_bilateral_game.edge_formation))
let () = printf "\n%f\n" (UBNash.all_player_compute ~cost ~game:emp ~network:(Network.build emp ~f:Undirected_bilateral_game.edge_formation))



