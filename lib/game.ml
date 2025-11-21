open Core

module type S = sig 
  type t = bool array array
  val value_function : int option -> float
  val edge_formation : bool -> bool -> bool
end

let inf = 1000000000.0

let contribute (con : bool array array) i j =
  if i <> j then con.(i).(j) <- true

let uncontribute (con : bool array array) i j =
  if i <> j then con.(i).(j) <- false

let single_player_cost ~(cost : float) (con : bool array array) (p : int) =
  Array.fold con.(p) ~init:0.0 ~f:(fun acc v -> if v then acc +. cost else acc)

let all_player_cost ~(cost : float) (con : bool array array) =
  Array.foldi con ~init:0.0 ~f:(fun i acc _ ->
    acc +. (single_player_cost ~cost con i)
  )
