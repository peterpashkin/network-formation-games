open Core
type t = bool array array

let discount_factor : float = 0.4

let value_function (dist : int option) =
  match dist with
  | None -> 0.0
  | Some d -> discount_factor ** float_of_int d

let edge_formation = (&&)



