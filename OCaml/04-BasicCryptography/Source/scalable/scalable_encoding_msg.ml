(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power



(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec bitstr = function
    |n when n >= len -> []
    |n -> let code = from_int (Char.code str.[n]) in
          match code with
          |[] -> []
          |_::code -> bitstr (n + 1) @ code
  in 0::(bitstr 0);;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits = match msg with
  |[] -> ""
  |_::bA ->
    let rec n_el n = function
      |[] when n > 0 -> invalid_arg "Not enough bits"
      |[] -> []
      |bA when n = 0 -> []
      |e::bA -> e::(n_el (n - 1) bA)
    in
    let rec del_n_el n = function
      |[] when n > 0 -> invalid_arg "Not enough bits"
      |[] -> []
      |bA when n = 0 -> bA
      |_::bA -> del_n_el (n - 1) bA
    in
    let rec aux = function
      |[] -> ""
      |bA -> let ord = to_int (0::(n_el bits bA)) in
        let c = Char.escaped (Char.chr ord) in
             (aux (del_n_el bits bA)) ^ c
    in aux bA;;
