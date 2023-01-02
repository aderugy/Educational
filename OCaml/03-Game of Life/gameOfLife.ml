#use "list_tools.ml";;

#use "topfind";;
#require "graphics";;
open Graphics;;

let rules0 cell near =
  match (cell, near) with
  |(_, 3) | (1, 2) -> 1
  |(_, _) -> 0 ;;

let count_neighbours (x, y) board size =
  if size < 0 then invalid_arg "size too short"
  else

  let rec aux_x x row posX flag =
    match row with
    |[] -> 0
    |cell::row when x = posX && flag -> aux_x x row (posX + 1) flag
    |cell::row when (posX >= x - 1) && (posX <= x + 1) ->
      cell + aux_x x row (posX + 1) flag
    |_::row -> aux_x x row (posX + 1) flag
  in

  let rec aux_y (x, y) board posY =
    match board with
    |[] -> 0
    |row::board when (posY >= y - 1) && (posY <= y + 1) ->
      (aux_x x row 0 (y=posY)) + (aux_y (x, y) board (posY + 1))
    |_::board -> aux_y (x, y) board (posY + 1)

  in aux_y (x, y) board 0;;


let rec seed_life board size nb_cell =
  match nb_cell with
  |0 -> board
  |_ ->
    let x = Random.int size
    and y = Random.int size in
    let cell = get_cell (x, y) board in

    match cell with
    |0 -> seed_life (put_cell 1 (x, y) board) size (nb_cell - 1)
    |_ -> seed_life board size nb_cell;;


let new_board size nb_cell =
  seed_life
    (init_board (size, size) 0)
    size
    nb_cell ;;

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



let rec game board size n =
  let cell_size = 10 in
  match n with
  |0 -> draw_board board cell_size
  |n ->
    let new_board = next_generation board size
    in draw_board board cell_size ;
       game new_board size (n - 1) ;;

let new_game size nb_cell n =
  game (new_board size nb_cell) size n;;
