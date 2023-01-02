#use "list_tools.ml" ;;

(* EXERCISE 2 Positional notation *)

let rigit x list =
  let rec aux x n = function
    |[] -> failwith "out of base"
    |e::list -> if (e = x) then n else aux x (n + 1) list

  in aux x 0 list ;;

let reverse l =
  let rec aux l l2 =
    match l2 with
    |[] -> l
    |e::l2 -> aux (e::l) l2
  in aux [] l;;

let recompose rep base =
  let b = length base in
  let rec aux b rep base =
    match rep with
    |[] -> 0
    |e::rep -> rigit e base + b * aux b rep base

  in aux b (reverse rep) base ;;


let decompose x base =
  match x with
  |x when x < 0 -> invalid_arg "negative number"
  |x ->
    let rec aux b base = function
      |x when x = 0 -> []
      |x -> (nth (x mod b) base) :: aux b base (x / b)
    in reverse (aux (length base) base x) ;;
