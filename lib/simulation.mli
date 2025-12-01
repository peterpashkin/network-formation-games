open! Core

val run_undirected_bilateral_sim : Undirected_bilateral_game.t ->
    runs:int ->
    cost:float ->
    unit


val run_undirected_unilateral_sim : Undirected_unilateral_game.t ->
    runs:int ->
    cost:float ->
    unit