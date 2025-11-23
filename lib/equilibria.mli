module Make (G : Game.S) : sig
    val check_nash_pair : G.t ->
        cost:float ->
        strict:bool ->
        current_val:float ->
        int ->
        int ->
        bool
    val check_simple_nash : G.t -> cost:float -> strict:bool -> bool
    val pairwise_stability : G.t -> cost:float -> Action.t
    val check_pairwise_stable: G.t -> cost:float -> int -> int -> Network.t -> Action.t
    val single_player_compute : network:Network.t -> cost:float -> int -> float
    val all_player_compute : network:Network.t -> cost:float -> float
end

