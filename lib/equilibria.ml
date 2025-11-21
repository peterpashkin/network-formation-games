open Core

module Make (G : Game.S) = struct

  let single_player_compute ~network ~cost player =
    Network.((single_person_val ~f:G.value_function network player) -. (Game.single_player_cost ~cost network player))

  let check_simple_nash (con : G.t) ~cost ~strict =
    let current_network = Network.build con ~f:G.edge_formation in
    let n = Array.length con in
    let acc = ref true in
    for i = 0 to n - 1 do
      let current_val = single_player_compute ~network:current_network ~cost i in
      for j = 0 to n - 1 do
        if i <> j then (
          if con.(i).(j) then (
            let con_copy = Array.map con ~f:Array.copy in
            Game.uncontribute con_copy i j;
            let network = Network.build con_copy ~f:G.edge_formation in
            let nv2 = single_player_compute ~network ~cost i in
            acc := !acc && ((if strict then Float.(>) else Float.(>=)) current_val nv2)
          ) else (
            let con_copy = Array.map con ~f:Array.copy in
            Game.contribute con_copy i j;
            let network = Network.build con_copy ~f:G.edge_formation in
            let nv1 = single_player_compute ~network ~cost i in
            acc := !acc && ((if strict then Float.(>) else Float.(>=)) current_val nv1)
          )
        )
      done
    done;
    !acc

  let check_strict_improve cvi cvj nvi nvj =
    let open Float in
    nvi > cvi && nvj >= cvj || nvi >= cvi && nvj > cvj

  let check_pairwise_stable (con : G.t) ~cost i j (current_network : Network.t) : Action.t =
    if i = j then None else
    let current_val_i = single_player_compute ~network:current_network ~cost i in
    let current_val_j = single_player_compute ~network:current_network ~cost j in
    if con.(j).(i) then (
      let con_copy = Array.map con ~f:Array.copy in
      Game.uncontribute con_copy j i;
      Game.uncontribute con_copy i j;
      let network = Network.build con_copy ~f:G.edge_formation in
      let new_val_i = single_player_compute ~network ~cost i in
      let new_val_j = single_player_compute ~network ~cost j in
      if (not Float.(new_val_i > current_val_i || new_val_j > current_val_j))
        then Action.None else Action.Drop(i,j)
    ) else (
      let con_copy = Array.map con ~f:Array.copy in
      Game.contribute con_copy j i;
      Game.contribute con_copy i j;
      let network = Network.build con_copy ~f:G.edge_formation in
      let new_val_i = single_player_compute ~network ~cost i in
      let new_val_j = single_player_compute ~network ~cost j in
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