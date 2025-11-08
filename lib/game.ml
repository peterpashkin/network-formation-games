open Core

module type S = sig 
  type t = bool list list
  val value_function : int option -> float
  val edge_formation : bool -> bool -> bool
end
let inf = 1000000000.0

let contribute (con : bool list list) i j =
  if i = j then con
  else
  List.mapi con ~f:(fun idx row ->
    if idx = i then
      List.mapi row ~f:(fun jdx v -> if jdx = j then true else v)
    else
      row
  )

let uncontribute (con : bool list list) i j =
  if i = j then con
  else
  List.mapi con ~f:(fun idx row ->
    if idx = i then
      List.mapi row ~f:(fun jdx v -> if jdx = j then false else v)
    else
      row
  )

let single_player_cost  ~(cost : float) (con : bool list list) (p : int) =
  List.nth_exn con p |>
  List.fold ~init:0.0 ~f:(fun acc v -> if v then acc +. cost else acc)


let all_player_cost ~(cost : float) (con : bool list list) =
  List.foldi con ~init:0.0 ~f:(fun i acc _ ->
    acc +. (single_player_cost ~cost con i)
  )