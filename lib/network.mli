type t = bool array array

val init : int -> t
val build : bool array array -> f:(bool -> bool -> bool) -> t
val add_edge : t -> int -> int -> unit
val remove_edge : t -> int -> int -> unit
val single_person_val : f:(int option -> float) -> t -> int -> float
val complete_val : f:(int option -> float) -> t -> float
val neighbours : t -> int -> int list
