open! Core

let run_undirected_bilateral_sim (initial : Undirected_bilateral_game.t) ~(runs : int) ~cost : Undirected_bilateral_game.t =
  let rec aux (tries : int) game =
    if tries = 0 then game
    else
    let current_network = Network.build game ~f:Undirected_bilateral_game.edge_formation in
    let module E = Equilibria.Make(Undirected_bilateral_game) in
    let n = Array.length game in
    let i = Random.int n in
    let action = ref Action.None in
    for j = 0 to n - 1 do
      match !action with
      | Action.None -> action := E.check_pairwise_stable game ~cost i j current_network
      | _ -> ()
    done;
    match !action with
    | Action.None -> aux (tries - 1) game
    | Action.Sponsor (src, dst) ->
      Game.contribute game src dst;
      Game.contribute game dst src;
      aux (tries - 1) game
    | Action.Drop (src, dst) ->
      Game.uncontribute game src dst;
      Game.uncontribute game dst src;
      aux (tries - 1) game
  in
  aux runs initial