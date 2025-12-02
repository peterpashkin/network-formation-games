open Core

module Make (G : Game.S) = struct

  let single_player_compute ~game ~network ~cost player =
    Network.((single_person_val ~f:G.value_function network player) -. (Game.single_player_cost ~cost game player))

  let all_player_compute ~game ~network ~cost =
    let n = Array.length game in
    let rec aux i acc =
      if i = n then acc
      else aux (i + 1) (acc +. single_player_compute ~network ~game ~cost i)
    in
    aux 0 0.0


  let check_nash_pair (game: G.t) ~cost ~strict ~current_val i j  =
    if i <> j then (
      if game.(i).(j) then (
        let game_copy = Array.map game ~f:Array.copy in
        Game.uncontribute game_copy i j;
        let network = Network.build game_copy ~f:G.edge_formation in
        let nv2 = single_player_compute ~network ~game:game_copy ~cost i in
        if ((if strict then Float.(>) else Float.(>=)) current_val nv2)
        then Action.None else Action.Drop(i,j)
      ) else (
        let game_copy = Array.map game ~f:Array.copy in
        Game.contribute game_copy i j;
        let network = Network.build game_copy ~f:G.edge_formation in
        let nv1 = single_player_compute ~network ~game:game_copy ~cost i in
        if ((if strict then Float.(>) else Float.(>=)) current_val nv1)
        then Action.None else Action.Sponsor(i,j)
      )
    ) else Action.None


  let check_simple_nash (game : G.t) ~cost ~strict =
    let current_network = Network.build game ~f:G.edge_formation in
    let n = Array.length game in
    let acc = ref Action.None in
    for i = 0 to n - 1 do
      let current_val = single_player_compute ~game ~network:current_network ~cost i in
      for j = 0 to n - 1 do
        match !acc with
        | Action.None -> acc := check_nash_pair game ~cost ~strict i j ~current_val
        | _ -> ()
      done
    done;
    !acc

  let check_strict_improve cvi cvj nvi nvj =
    let open Float in
    nvi > cvi && nvj >= cvj || nvi >= cvi && nvj > cvj

  let check_pairwise_stable (game : G.t) ~cost i j (current_network : Network.t) : Action.t =
    if i = j then None else
    let current_val_i = single_player_compute ~game ~network:current_network ~cost i in
    let current_val_j = single_player_compute ~game ~network:current_network ~cost j in
    if game.(j).(i) then (
      Network.remove_edge current_network j i;
      Network.remove_edge current_network i j;
      Game.uncontribute game i j;
      Game.uncontribute game j i;
      let new_val_i = single_player_compute ~game ~network:current_network ~cost i in
      let new_val_j = single_player_compute ~game ~network:current_network ~cost j in
      Game.contribute game i j;
      Game.contribute game j i;
      Network.add_edge current_network j i;
      Network.add_edge current_network i j;
      if (Float.(new_val_i > current_val_i || new_val_j > current_val_j))
        then Action.Drop(i,j) else Action.None
    ) else (
      Network.add_edge current_network j i;
      Network.add_edge current_network i j;
      Game.contribute game i j;
      Game.contribute game j i;
      let new_val_i = single_player_compute ~game ~network:current_network ~cost i in
      let new_val_j = single_player_compute ~game ~network:current_network ~cost j in
      Game.uncontribute game i j;
      Game.uncontribute game j i;
      Network.remove_edge current_network j i;
      Network.remove_edge current_network i j;
      if (not (check_strict_improve current_val_i current_val_j new_val_i new_val_j)) then Action.None else Action.Sponsor (i,j)
    )

  let pairwise_stability (con : G.t) ~cost =
    let current_network = Network.build con ~f:G.edge_formation in
    let n = Array.length con in
    let result = ref Action.None in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        match !result with
        | Action.None -> result := check_pairwise_stable con ~cost i j current_network
        | _ -> ()
      done
    done;
    !result
end