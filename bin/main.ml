open Core
open Sims

let transpose m =
  List.init (List.length m) ~f:(fun i ->
    List.init (List.length m) ~f:(fun j ->
      List.nth_exn (List.nth_exn m j) i
    )
  )

let combine m1 m2 =
  List.map2_exn m1 m2 ~f:(fun row1 row2 ->
    List.map2_exn row1 row2 ~f:(fun v1 v2 -> v1 || v2)
  )

let two_sided g = 
  combine g (transpose g)

let _chain sz =
  List.init sz ~f:(fun i ->
    List.init sz ~f:(fun j ->j = i + 1)
  )


let _star sz =
  List.init sz ~f:(fun i ->
    if i = sz-1 then
      List.init sz ~f:((<>) (sz - 1))
    else
      List.init sz ~f:(fun _ -> false)
  )

let _complete sz =
  List.init sz ~f:(fun i ->
    List.init sz ~f:(fun j -> j > i)
  )


let _wheels sz =
  List.init sz ~f:(fun i ->
    List.init sz ~f:(fun j -> if i = sz-1 then j = 0 else j = i + 1)
  )

module UUNash = Equilibria.Make (Undirected_unilateral_game)
module UBNash = Equilibria.Make (Undirected_bilateral_game)
module DUNash = Equilibria.Make (Directed_unilateral_game)

let () = UUNash.check_nash (_star 20) ~cost:0.8 ~strict:false |> printf "Is Nash equilibrium for Undirected Unilateral: %b\n"
let () = DUNash.check_nash (_wheels 20) ~cost:25. ~strict:false |> printf "Is Nash equilibrium for Directed Unilateral: %b\n"
let () = UBNash.pairwise_stability (_star 20 |> two_sided) ~cost:0.8 |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n"