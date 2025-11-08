type t = bool list list

val init : int -> t
val build : bool list list -> f:(bool -> bool -> bool) -> t
val add_edge : t -> int -> int -> t
val single_person_val : f:(int option -> float) -> t -> int -> float
val complete_val : f:(int option -> float)  -> t -> float
