#use "list_tools.ml";;

(* EXERCICE 3 *)

let insert_list list rank sublist =
  match rank with
  |rank when rank <= 0 -> invalid_arg "insert_list: invalid rank"
  |rank ->
    let rec aux list rank sublist x =
      match list with
      |[] when rank = x -> sublist
      |[] -> failwith "insert_list: list too short"
      |e::list when rank = x -> append sublist (e::list)
      |e::list -> e :: aux list rank sublist (x + 1)
    in
    aux list (rank - 1) sublist 0;;

let rec insert_list_multiple ml sl list_rank =
  match list_rank with
  |[] -> []
  |e::list_rank ->
    if (list_rank = [])
    then insert_list ml e sl
    else insert_list (insert_list_multiple ml sl list_rank) e sl;;
