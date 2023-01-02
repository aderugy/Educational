# use "list_tools.ml" ;;

(* Uniquely decodable *)

let is_prefix plist list =

  let rec aux plist list x =
    if x >= 0 then
      let l = nth x list
      in prefix plist l || aux plist list (x - 1)
    else false

  in (length list > 0) && aux plist list ((length list) - 1);;


let rec decodable codes =
  match codes with
  |[] -> true
  |e::l ->
    not (is_prefix e l) && decodable l ;;
