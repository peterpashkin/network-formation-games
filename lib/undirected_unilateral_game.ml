open Core
type t = bool array array

let value_function (dist : int option) : float =
  match dist with
  | None -> -.Game.inf
  | Some d -> float_of_int (-d)

let edge_formation = (||)