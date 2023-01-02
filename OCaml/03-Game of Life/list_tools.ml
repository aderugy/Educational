#use "topfind";;
#require "graphics";;
open Graphics;;

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

let init_list n x =
  match n with
  |n when n < 0 -> invalid_arg "init_list: n must be a natural"
  |n ->
    let rec aux n x =
      match n with
      |0 -> []
      |_ -> x :: aux (n - 1) x
    in aux n x;;

let put_list v i list =
  match i with
  |i when i < 0 || i >= length list ->
    failwith "putlist: index i out of bounds"
  |_ ->
    let rec aux v i = function
      |[] -> []
      |e::l ->
        (if i = 0 then v::l else e::(aux v (i - 1) l))
    in aux v i list ;;

let init_board (l, c) x =
  init_list c (init_list l x) ;;

let get_cell (x, y) board =
  nth x (nth y board);;

let put_cell v (x, y) board =
  put_list (put_list v x (nth y board)) y board ;;

let open_window size =
  open_graph (" " ^ string_of_int size ^ "x"
              ^ string_of_int (size + 20)) ;;

let draw_cell (x, y) size color =
  set_color (rgb 127 127 127);
  draw_rect x y size size;
  set_color color;
  fill_rect (x + 1) (y + 1) (size - 2) (size - 2) ;;

let cell_color = function
              |0 -> white
              |_ -> black;;

let rec draw_board board cellsize =

  let rec aux row (x, y) cellsize =
    match row with
    |[] -> ()
    |cell::row ->
      begin
        draw_cell (x, y) cellsize (cell_color cell) ;
        aux row (x, (y + cellsize)) cellsize
      end
  in

  let rec aux_columns board cellsize n =
    match board with
    |[] -> ()
    |row::board ->
      begin
        aux_columns board cellsize (n + 1) ;
        aux row ((n * cellsize), 0) cellsize
      end
  in aux_columns board cellsize 0 ;;
