open Core

let transpose m =
  let n = Array.length m in
  Array.init n ~f:(fun i ->
    Array.init n ~f:(fun j -> m.(j).(i))
  )

let combine m1 m2 =
  Array.map2_exn m1 m2 ~f:(fun row1 row2 ->
    Array.map2_exn row1 row2 ~f:(fun v1 v2 -> v1 || v2)
  )

let two_sided g = 
  combine g (transpose g)

let chain sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> j = i + 1)
  )

let star sz =
  Array.init sz ~f:(fun i ->
    if i = sz - 1 then
      Array.init sz ~f:(fun j -> j <> (sz - 1))
    else
      Array.init sz ~f:(fun _ -> false)
  )

let complete sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> j > i)
  )

let wheels sz =
  Array.init sz ~f:(fun i ->
    Array.init sz ~f:(fun j -> if i = sz - 1 then j = 0 else j = i + 1)
  )

let empty sz = 
  Array.make_matrix ~dimx:sz ~dimy:sz false


let rec add_random_edges g edges_added total_edges sz ~single =
  if edges_added >= total_edges then g
  else
    let src = Random.int sz in
    let dst = Random.int sz in
    if src = dst then add_random_edges g edges_added total_edges sz ~single
    else
      if g.(src).(dst) then
        add_random_edges g edges_added total_edges sz ~single
      else (
        Game.contribute g src dst;
        if not single then Game.contribute g dst src;
        add_random_edges g (edges_added + 1) total_edges sz ~single
      )

(* Create [n_c] cliques of size [c_sz] and graph of size [sz] 
  then sparsely connect these cliques (aka add alpha*n) random edges
  *)
let common_bilateral ~sz ~c_sz ~n_c ~alpha = 
  let init = empty sz in
  let create_clique g fst =
    let all = List.init c_sz ~f:(fun i -> fst + i) in
    List.iter all ~f:(fun i ->
      List.iter all ~f:(fun j ->
        if i <> j then Game.contribute g i j
      )
    );
    g
  in
  let with_cliques = 
    List.foldi (List.init n_c ~f:Fun.id) ~init ~f:(fun i acc _ ->
      create_clique acc (i * c_sz)
    )
  in
  let total_edges = sz * alpha in
  add_random_edges with_cliques 0 total_edges sz



  (* TODO maye false *)
let full_cliques ~sz ~c_sz =
  let init = empty sz in
  let rec aux g current =
    if current >= sz then g
    else
      let all = List.init c_sz ~f:(fun i -> current + i) in
      List.iter all ~f:(fun i ->
        List.iter all ~f:(fun j ->
          if i <> j then Game.contribute g i j
        )
      );
      aux g (current + c_sz)
  in
  aux init 0



let sparsely_connected_cliques ~sz ~c_sz ~total_edges =
  let g = full_cliques ~sz ~c_sz in
  add_random_edges g 0 total_edges sz ~single:false


let some_stars ~sz ~star_sz =
  let init = empty sz in
  let rec aux g current =
    if current >= sz then g
    else
      let all = List.init star_sz ~f:(fun i -> current + i) in
      List.iter all ~f:(fun j ->
       if current <> j then Game.contribute g current j
      );
      aux g (current + star_sz)
  in
  aux init 0


let connected_stars ~sz ~star_sz ~total_edges =
  let g = some_stars ~sz ~star_sz in
  add_random_edges g 0 total_edges sz ~single:true


let print_graph g ~uni =
  let n = Array.length g in
  (* for i = 0 to n - 1 do
    printf "%d\n" i; 
  done; *)
  for i = 0 to n - 1 do
    for j = (if uni then 0 else i+1) to n - 1 do
      if g.(i).(j) then
        printf "%d %d\n" i j
    done;
  done;