(* 1.1 *)
let rec length = function
  |[] -> 0
  |e::l -> 1 + length l;;

let nth n list =
    match n with
    |n when n < 0 -> invalid_arg "nth: index must be a natural"
    |n ->
      let rec aux n l =
        match l with
        |[] -> failwith "nth: list is too short"
        |e::l when n = 0 -> e
        |_::l -> aux (n - 1) l
      in aux n list;;

let prefix list1 list2 =
  let rec aux l1 = function
    |[] -> true
    |e2::l2 ->
      match l1 with
      |[] -> true
      |e1::l1 ->
        e1 = e2 && aux l1 l2
  in if (length list1 < length list2)
     then aux list1 list2
     else aux list2 list1;;


(* 1.2 *)

let init_list n x =
  match n with
  |n when n < 0 -> invalid_arg "init_list: n must be a natural"
  |n ->
    let rec aux n x =
      match n with
      |0 -> []
      |_ -> x :: aux (n - 1) x
    in aux n x;;


let rec append l1 l2 =
  match l1 with
  |[] -> l2
  |e::l1 -> e::(append l1 l2) ;;


let put_list v i list =
  match i with
  |i when i < 0 || i >= length list ->
    failwith "putlist: index i out of bounds"
  |_ ->
    let rec aux v i = function
      |[] -> []
      |e::l ->
        (if i = 0 then v else e)::(aux v (i - 1) l)
    in aux v i list ;;


(* 1.3 *)

let init_board (l, c) x =
  init_list c (init_list l x) ;;

let get_cell (x, y) board =
  nth x (nth y board);;

let put_cell v (x, y) board =
  put_list (put_list v x (nth y board)) y board ;;
