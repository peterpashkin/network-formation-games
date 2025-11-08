open Core
open Sims
let _chain_one_side sz =
  List.init sz ~f:(fun i ->
    List.init sz ~f:(fun j ->j = i + 1)
  )

let _star_one_side sz =
  List.init sz ~f:(fun i ->
    if i = sz-1 then
      List.init sz ~f:((<>) (sz - 1))
    else
      List.init sz ~f:(fun _ -> false)
  )

let _complete_one_sided sz =
  List.init sz ~f:(fun i ->
    List.init sz ~f:(fun j -> j > i)
  )

module MyNash = Nash.Make (Undirected_unilateral_game)

let () = MyNash.check_nash (_star_one_side 20) ~cost:1.2 |> printf "Is Nash equilibrium: %b\n"