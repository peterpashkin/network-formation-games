module type S = sig 
    type t = bool array array
    val value_function : int option -> float
    val edge_formation : bool -> bool -> bool
end
val inf : float
val contribute : bool array array -> int -> int -> unit
val uncontribute : bool array array -> int -> int -> unit
val single_player_cost : cost:float -> bool array array -> int -> float

