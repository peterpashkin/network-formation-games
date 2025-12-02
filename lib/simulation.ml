open! Core

let run_undirected_bilateral_sim (initial : Undirected_bilateral_game.t) ~(runs : int) ~cost =
  let rec aux (tries : int) game =
    if tries = 0 then ()
    else
    let current_network = Network.build game ~f:Undirected_bilateral_game.edge_formation in
    let module E = Equilibria.Make(Undirected_bilateral_game) in
    let n = Array.length game in
    let i = Random.int n in
    let j = Random.int n in
    if i = j then aux tries game
    else
      match E.check_pairwise_stable game ~cost i j current_network with
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


let run_undirected_unilateral_sim (initial : Undirected_unilateral_game.t) ~(runs : int) ~cost =
  let rec aux (tries : int) game =
    if tries = 0 then ()
    else
    let current_network = Network.build game ~f:Undirected_unilateral_game.edge_formation in
    let module E = Equilibria.Make(Undirected_unilateral_game) in
    let n = Array.length game in
    let i = Random.int n in
    let j = Random.int n in
    if i = j then aux tries game
    else 
      let current_val = E.single_player_compute ~game ~network:current_network ~cost i in
      match (E.check_nash_pair game ~cost ~strict:false i j ~current_val) with
      | Action.None -> aux (tries - 1) game
      | Action.Sponsor (src, dst) ->
        Game.contribute game src dst;
        aux (tries - 1) game
      | Action.Drop (src, dst) ->
        Game.uncontribute game src dst;
        aux (tries - 1) game
  in
  aux runs initial