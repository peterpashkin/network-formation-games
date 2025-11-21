open Core
module Make (G : Game.S) = struct

  let single_player_compute ~network ~cost player =
    Network.((single_person_val ~f:G.value_function network player) -. (Game.single_player_cost ~cost network player))

    (* TODO not correct nash check, need to check all other action profiles *)
  let check_simple_nash (con : G.t) ~cost ~strict =
    let current_network = Network.build con ~f:G.edge_formation in
    List.foldi con ~init:true ~f:( fun i acc row ->
      let current_val = single_player_compute ~network:current_network ~cost i in
      List.foldi row ~init:acc ~f:(fun j acc v ->
          if i = j then acc else
          if v then
            let network = Network.build (Game.uncontribute con i j) ~f:G.edge_formation in
            let nv2 = single_player_compute ~network ~cost i in
            acc && ((if strict then Float.(>) else Float.(>=)) current_val nv2)
          else
            let network = Network.build (Game.contribute con i j) ~f:G.edge_formation in
            let nv1 = single_player_compute ~network ~cost i in
            acc && ((if strict then Float.(>) else Float.(>=)) current_val nv1)
      )
      )

  
  let check_strict_improve cvi cvj nvi nvj =
    let open Float in
    nvi > cvi && nvj >= cvj || nvi >= cvi && nvj > cvj 


  let check_pairwise_stable (con : G.t) ~cost i j (current_network : Network.t) : Action.t =
    if i = j then None else
    let current_val_i = single_player_compute ~network:current_network ~cost i in
    let current_val_j = single_player_compute ~network:current_network ~cost j in
    if List.nth_exn (List.nth_exn con j) i then
      let network = Network.build (Game.uncontribute (Game.uncontribute con j i) i j) ~f:G.edge_formation in
      let new_val_i = single_player_compute ~network ~cost i in
      let new_val_j = single_player_compute ~network ~cost j in
      if (not Float.(new_val_i > current_val_i || new_val_j > current_val_j))
        then Action.None else Action.Drop(i,j)
    else
      let network = Network.build (Game.contribute (Game.contribute con j i) i j) ~f:G.edge_formation in
      let new_val_i = single_player_compute ~network ~cost i in
      let new_val_j = single_player_compute ~network ~cost j in
      if (not (check_strict_improve current_val_i current_val_j new_val_i new_val_j)) then Action.None else Action.Sponsor (i,j)
    


  let pairwise_stability (con : G.t) ~cost =
    let current_network = Network.build con ~f:G.edge_formation in
    List.foldi con ~init:Action.None ~f:( fun i acc row ->
      List.foldi row ~init:acc ~f:(fun j acc _ ->
          match acc with Action.None ->
          check_pairwise_stable con ~cost i j current_network
          | x -> x
      )
    )
end