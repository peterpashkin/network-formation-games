open Core 

type t = bool array array

let add_edge (g: t) (src: int) (dst: int) : unit =
  g.(src).(dst) <- true

let init (size : int) : t =
  Array.make_matrix ~dimx:size ~dimy:size false

let get g i j = 
  g.(i).(j)

let build cont ~f = 
  let initial = init (Array.length cont) in
  Array.iteri cont ~f:(fun i row ->
    Array.iteri row ~f:(fun j _ ->
      if f (get cont i j) (get cont j i) then add_edge initial i j
    )
  );
  initial

let neighbours (g : t) (p : int) = 
  Array.foldi g.(p) ~init:[] ~f:(fun j acc v -> 
    if v then j :: acc else acc
  ) |> List.rev

let dist (g : t) (p1 : int) (p2 : int) = 
  let visited = Array.init (Array.length g) ~f:(fun _ -> false) in
  let rec bfs queue visited =
    match queue with
    | [] -> None
    | (current, d) :: rest ->
        visited.(current) <- true;
        if current = p2 then Some d
        else
          let neigh = neighbours g current in
          let new_neighbors = List.filter neigh ~f:(fun n -> not visited.(n)) in
          let new_queue = rest @ (List.map new_neighbors ~f:(fun n -> (n, d + 1))) in
          bfs new_queue visited
  in
  bfs [(p1, 0)] visited

let single_person_val ~f (g : t) (p : int) =
  let tries = List.init (Array.length g) ~f:(fun i -> i) |> List.filter ~f:((<>) p) in
  List.fold tries ~init:0.0 ~f:(fun acc other ->
    acc +. f (dist g p other)
  )

let complete_val ~f (g : t) =
  List.init (Array.length g) ~f:Fun.id |> List.fold ~init:0.0 ~f:(fun acc p ->
    acc +. single_person_val ~f g p
  )