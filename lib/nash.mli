module Make (G : Game.S) : sig
    val check_nash : G.t -> cost:float -> bool
end