module Make (G : Game.S) : sig
    val check_nash_pair : G.t ->
        cost:float ->
        strict:bool ->
        current_val:float ->
        int ->
        int ->
        Action.t
    val check_simple_nash : G.t -> cost:float -> strict:bool -> Action.t
    val pairwise_stability : G.t -> cost:float -> Action.t
    val check_pairwise_stable: G.t -> cost:float -> int -> int -> Network.t -> Action.t
    val single_player_compute : game:G.t -> network:Network.t -> cost:float -> int -> float
    val all_player_compute : game:G.t -> network:Network.t -> cost:float -> float
end

