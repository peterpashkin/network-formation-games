open Core 

type t = bool list list

let add_edge (g: t) (src: int) (dst: int) : t =
  List.mapi ~f:(fun i row ->
    if i = src then
      List.mapi ~f:(fun j v -> if j = dst then true else v) row
    else
      row
    ) g

let init (size : int) : t =
  List.init size ~f:(fun _ -> List.init size ~f:(fun _ -> false))

let get g i j = 
  List.nth_exn (List.nth_exn g i) j

let build cont ~f = 
  let initial = init (List.length cont) in
  List.foldi cont ~init:initial ~f:(fun i acc row ->
    List.foldi row ~init:acc ~f:(fun j acc _ ->
      if f (get cont i j) (get cont j i) then add_edge acc i j else acc
    )
  )


let neighbours (g : t) (p : int) = 
  List.filter_mapi (List.nth_exn g p) ~f:(fun j v -> 
    if v then Some j else None
  )

let dist (g : t) (p1 : int) (p2 : int) = 
  let visited = Array.init (List.length g) ~f:(fun _ -> false) in
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
  let tries = List.init (List.length g) ~f:(fun i -> i) |> List.filter ~f:((<>) p) in
  List.fold tries ~init:0.0 ~f:(fun acc other ->
    acc +. f (dist g p other)
  )

let complete_val ~f (g : t)  =
  List.foldi g ~init:0.0 ~f:(fun i acc row ->
    List.foldi row ~init:acc ~f:(fun j acc _ ->
      if j <= i then acc else
      acc +. f (dist g i j))
  )
