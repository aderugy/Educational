#use "topfind";;
#require "graphics";;

open Graphics;;
open Random;;

(* constant for size *)
let size = 50;;

(* constant for cell size *)
let cellsize = 20;;

(* constant for window size *)
let window_size = size*cellsize;;

open_window window_size;;

let rec length = function
  |[] -> 0
  |e::l -> 1 + length l;;

let nth n list =  match n with
    |n when n < 0 -> invalid_arg "nth: index must be a natural"
    |n ->
      let rec aux n = function
        |[] -> failwith "nth: list is too short"
        |e::l when n = 0 -> e
        |_::l -> aux (n - 1) l
      in aux n list;;

let init_list n x = match n with
  |n when n < 0 -> invalid_arg "init_list: n must be a natural"
  |n ->
    let rec aux n x = match n with
      |0 -> []
      |_ -> x :: aux (n - 1) x
    in aux n x;;

let put_list v i list = match i with
  |i when i < 0 || i >= length list ->
    failwith "putlist: index i out of bounds"
  |_ ->
    let rec aux v i = function
      |[] -> []
      |e::l ->
        (if i = 0 then v::l
         else e::(aux v (i - 1) l))
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

let cell_color = function
              |0 -> white
              |_ -> black;;

let draw_cell (x, y) size color =
  set_color (rgb 127 127 127);
  draw_rect x y size size;
  set_color color;
  fill_rect (x + 1) (y + 1) (size - 2) (size - 2) ;;

let draw_board board nboard cellsize flag =
  let rec aux_x (x, y) cellsize flag = function
    |([], _) | (_, []) -> ()
    |(cell::row, ncell::nrow) ->
      (
        if (cell <> ncell || flag)
        then draw_cell (x, y) cellsize (cell_color ncell)
        else ()
      ) ;
      aux_x (x, (y + cellsize)) cellsize flag (row, nrow) in

  let rec aux_y x cellsize flag = function
    |([], _) | (_, []) -> ()
    |(row::board, nrow::nboard) ->
      (
        aux_x (x, 0) cellsize flag (row, nrow) ;
        aux_y (x + cellsize) cellsize flag (board, nboard)
      ) in
  aux_y 0 cellsize flag (board, nboard);;


(* Formats a matrix  to the string format :
 values of the elements are stuck together, nl at each list*)
let rec sb = function
  |[] -> ""
  |row::board ->
    let rec aux = function
      |[] -> ""
      |e::row when row = [] -> string_of_int e
      |e::row -> (string_of_int e) ^ " " ^ aux row
    in (aux row) ^ (if board = [] then "" else "\n" ^ sb board);;

(* Writes a string to the file at path path *)
let write path str =
  let oc = open_out path in
  Printf.fprintf oc "%s" str ;
  close_out oc;;

(* Returns a list of strings of the lines of the file name *)
let load name =
  let ic = open_in name in
  let try_read () =
    try Some ( input_line ic )
    with End_of_file -> None in

  let rec loop () = match try_read () with
      Some s -> s::(loop ())
    | None -> close_in ic ;
              []
  in loop () ;;

let soc str =
  let length = String.length str in
  let rec aux str length = function
    |n when n >= length -> []
    |n -> let c = str.[n] in
          match c with
          |'0' | '1' -> (Char.escaped c)::(aux str length (n + 1))
          |_ -> aux str length (n + 1) in
  aux str length 0;;

(* Loads the board using format :
all numbers stuck together *)
let load_board path =
  let file = load path in
  let rec aux = function
    |[] -> []
    |str::file ->
      (soc str)::(aux file)
  in aux file;;

(* Saves the board to the file of path path *)
(* format : all numbers of each list are append,
every list comes after a new line *)
let save_board path board =
  write path (sb board);;

(* Rules of the cell *)
let rules0 cell near = match (cell, near) with
  |(_, 3) | (1, 2) -> 1
  |(_, _) -> 0 ;;

(* COUNTS THE ALIVE NEIGHBOURS OF EACH CELL *)
(* ONLY RUNS THROUGH THE MATRIX ONCE *)
let count_neighbours (x, y) board size =
  if size < 0 then invalid_arg "size too short" else
  let rec aux_x x row posX flag =  match row with
    |[] -> 0
    |cell::row when x = posX && flag -> aux_x x row (posX + 1) flag
    |cell::row when (posX >= x - 1) && (posX <= x + 1) ->
      cell + aux_x x row (posX + 1) flag
    |_::row -> aux_x x row (posX + 1) flag
  in
  let rec aux_y (x, y) board posY = match board with
    |[] -> 0
    |row::board when (posY >= y - 1) && (posY <= y + 1) ->
      (aux_x x row 0 (y=posY)) + (aux_y (x, y) board (posY + 1))
    |_::board -> aux_y (x, y) board (posY + 1)

  in aux_y (x, y) board 0;;

(* Creates a new board of (size * size) using nb_cells cells *)
let rec seed_life board size  = function
  |0 -> board
  |nb_cell ->
    let x = Random.int size
    and y = Random.int size in
    let cell = get_cell (x, y) board in

    match cell with
    |0 -> seed_life (put_cell 1 (x, y) board) size (nb_cell - 1)
    |_ -> seed_life board size nb_cell;;


(* returns a matrix of size*size with nb_cell
 alive cells placed randomly *)
let new_board size nb_cell =
  seed_life (init_board (size, size) 0) size nb_cell ;;

(* takes a board as parameter, returns the board modified
 according to the rules of game of life (next gen)*)
let next_generation board size =
  let rec aux_x board row (x, y) size = match row with
    |[] -> []
    |cell::row ->
      (rules0 cell (count_neighbours (x, y) board size))
      ::aux_x board row ((x + 1), y) size

  in let rec aux_y board b1 y size =
    match b1 with
    |[] -> []
    |row::b1 ->
      (aux_x board row (0, y) size)::(aux_y board b1 (y + 1) size)
  in aux_y board board 0 size;;


(* Starts the game using a defined board with n gen *)
let rec game board size n =
  match n with
  |0 -> draw_board board board cellsize true
  |n ->
    let nboard = next_generation board size
    in begin
        draw_board board nboard cellsize false ;
        game nboard size (n - 1)
      end;;

(* Starts a game using a new board
(nb_cells cells placed randomly, size size) *)
let new_game size nb_cell n =
  game (new_board size nb_cell) size n;;

(* Creates a board of size size and places cells at coords in pattern *)
let init_pattern pattern size =
  let board = init_board (size, size) 0 in

  let rec aux board = function
    |[] -> board
    |(x, y)::pattern -> aux (put_cell 1 (x, y) board) pattern
  in aux board pattern;;

(* Creates a game using a board created with
the pattern as the board parameter*)
let new_game_pattern board size nb =
  game (init_pattern board size) size nb;;

(* CHECKS WHETHER THERE ARE CELLS REMAINING *)
let rec remaining board =
  let rec aux = function
    |[] -> false
    |e::l -> (e = 1) || (aux l) in

  match board with
  |[] -> false
  |e::board -> (aux e) || (remaining board);;

(* Starts a Game of Life in a size * size matrix using nb_cells cells *)
(* DOESN'T DRAW THE GRAPH EACH GEN *)
let new_game_survival size nb_cells =
  let board = new_board size nb_cells in
  draw_board board board cellsize true ;

  let rec aux board size =
    if remaining board
    then
      (
        let nboard = next_generation board size in
        draw_board board nboard cellsize false ;
        aux nboard size
      )
    else ()
  in aux board size;;

(* Starts a game of life with predefined board in survival *)
let rec new_game_pattern_survival board size =
  draw_board board board cellsize true ;

  let rec aux board nboard size =
    if (remaining board)
    then
      (
        draw_board board nboard cellsize false ;
        aux nboard (next_generation board size) size
      )
    else draw_board board board cellsize true
  in aux board board size;;
