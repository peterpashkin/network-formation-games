open Core
module Make (G : Game.S) = struct
  let check_nash (con : G.t) ~cost =
    List.foldi con ~init:true ~f:( fun i acc row ->
      let current_network = Network.build con ~f:G.edge_formation in
      let current_val = Network.((single_person_val ~f:G.value_function current_network i) -. (Game.single_player_cost ~cost current_network i)) in
      List.foldi row ~init:acc ~f:(fun j acc _ ->
          let nn1 = Network.build (Game.contribute con i j) ~f:G.edge_formation in
          let nv1 = Network.((single_person_val ~f:G.value_function nn1 i) -. (Game.single_player_cost ~cost nn1 i)) in
          let nn2 = Network.build (Game.uncontribute con i j) ~f:G.edge_formation in
          let nv2 = Network.((single_person_val ~f:G.value_function nn2 i) -. (Game.single_player_cost ~cost nn2 i)) in
          acc && (Float.(>=) current_val nv1) && (Float.(>=) current_val nv2)
      )
    )
end