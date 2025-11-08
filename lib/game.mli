module type S = sig 
    type t = bool list list
    val value_function : int option -> float
    val edge_formation : bool -> bool -> bool
end
val inf : float
val contribute : bool list list -> int -> int -> bool list list
val uncontribute : bool list list -> int -> int -> bool list list
val single_player_cost : cost:float -> bool list list -> int -> float
val all_player_cost : cost:float -> bool list list -> float

