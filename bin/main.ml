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

let _empty sz = 
  List.init sz ~f:(fun _ ->
    List.init sz ~f:(fun _ -> false)
  )

(* Create [n_c] cliques of size [c_sz] and graph of size [sz] 
  then sparsely connect these cliques (aka add alpha*n) random edges
  *)
let _common_bilateral ~sz ~c_sz ~n_c ~alpha = 
  let init = _empty sz in
  let create_clique g fst =
    let all = List.init c_sz ~f:(fun i -> fst + i) in
    List.fold all ~init:g ~f:(fun acc i ->
      List.fold all ~init:acc ~f:(fun acc2 j ->
        if i = j then acc2
        else
        Game.contribute acc2 i j))
  in
  let with_cliques = 
    List.foldi (List.init n_c ~f:Fun.id) ~init ~f:(fun i acc _ ->
      create_clique acc (i * c_sz)
    )
  in
  let total_edges = sz * alpha in
  let rec add_random_edges g edges_added =
    if edges_added >= total_edges then g
    else
      let src = Random.int sz in
      let dst = Random.int sz in
      if src = dst then add_random_edges g edges_added
      else
        if List.nth_exn (List.nth_exn g src) dst then
          add_random_edges g edges_added
        else
          let g2 = Game.contribute g src dst in
          let g3 = Game.contribute g2 dst src in
          add_random_edges g3 (edges_added + 1)
  in
  add_random_edges with_cliques 0


module UUNash = Equilibria.Make (Undirected_unilateral_game)
module UBNash = Equilibria.Make (Undirected_bilateral_game)
module DUNash = Equilibria.Make (Directed_unilateral_game)

(* let () = UUNash.check_simple_nash (_star 20) ~cost:1.2 ~strict:false |> printf "Is Nash equilibrium for Undirected Unilateral: %b\n"
let () = DUNash.check_simple_nash (_wheels 20) ~cost:25. ~strict:false |> printf "Is Nash equilibrium for Directed Unilateral: %b\n" *)
let () = UBNash.pairwise_stability (_star 10 |> two_sided) ~cost:0.92 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n"

let () = UBNash.pairwise_stability (_common_bilateral ~sz:10 ~c_sz:0 ~n_c:0 ~alpha:0) ~cost:0.8 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n"
let () = UBNash.pairwise_stability (_empty 10) ~cost:0.8 |> (function Action.None -> true | _ -> false) |> printf "Is Nash equilibrium for Undirected Bilateral: %b\n"