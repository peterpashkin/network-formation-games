open! Core

let run_undirected_bilateral_sim (initial : Undirected_bilateral_game.t) ~(runs : int) ~cost : Undirected_bilateral_game.t =
  let rec aux (tries : int) game =
    if tries = 0 then game
    else
    let current_network = Network.build game ~f:Undirected_bilateral_game.edge_formation in
    let module E = Equilibria.Make(Undirected_bilateral_game) in
    let n = List.length game in
    let i = Random.int n in
    let action = List.foldi (List.nth_exn game i) ~init:Action.None ~f:(fun j acc _ ->
        match acc with Action.None ->
          E.check_pairwise_stable game ~cost i j current_network
        | x -> x
      ) in
    match action with
    | Action.None -> aux (tries - 1) game
    | Action.Sponsor (src, dst) ->
      let x = Game.contribute game src dst in
      let y = Game.contribute x dst src in
      aux (tries - 1) y
    | Action.Drop (src, dst) ->
        let x = Game.uncontribute game src dst in
        let y = Game.uncontribute x dst src in
        aux (tries - 1) y

  in
  aux runs initial