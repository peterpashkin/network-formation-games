type t = bool array array

let value_function (dist : int option) =
  match dist with
  | None -> 0.0
  | Some _ -> 1.0

let edge_formation a _ = a

