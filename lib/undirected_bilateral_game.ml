open Core
type t = bool list list
let discount_factor : float = 0.9
let value_function (dist : int option) =
  match dist with
  | None -> 0.0
  | Some d -> discount_factor ** float_of_int d

let edge_formation = (&&)
  


  