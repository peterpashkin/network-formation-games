(* testing results from paper to be true *)
open Core
open Sims

module UUNash = Equilibria.Make (Undirected_unilateral_game)
module UBNash = Equilibria.Make (Undirected_bilateral_game)

let size = 15

let complete_graph = Graph_gen.complete size
let star_graph = Graph_gen.star size
let empty_graph = Graph_gen.empty size

(* asserting for undirected unilateral *)
let () = assert (UUNash.check_simple_nash complete_graph ~cost:0.5 ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash complete_graph ~cost:1. ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash star_graph ~cost:1. ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash star_graph ~cost:1.5 ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash star_graph ~cost:2. ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash star_graph ~cost:2.5 ~strict:false |> (function Action.None -> true | _ -> false))
let () = assert (UUNash.check_simple_nash star_graph ~cost:100. ~strict:false |> (function Action.None -> true | _ -> false))



(* asserting undirected bilateral, delta = 0.4 *)
let () = assert (UBNash.pairwise_stability (Graph_gen.two_sided complete_graph) ~cost:0.1 |> (function Action.None -> true | _ -> false))
let () = assert (UBNash.pairwise_stability (Graph_gen.two_sided star_graph) ~cost:0.3 |> (function Action.None -> true | _ -> false))
let () = assert (UBNash.pairwise_stability (Graph_gen.two_sided empty_graph) ~cost:0.5 |> (function Action.None -> true | _ -> false))
let () = assert (not (UBNash.pairwise_stability (Graph_gen.two_sided star_graph) ~cost:0.5 |> (function Action.None -> true | _ -> false)))
let () = assert (UBNash.pairwise_stability (Graph_gen.two_sided empty_graph) ~cost:100. |> (function Action.None -> true | _ -> false))
