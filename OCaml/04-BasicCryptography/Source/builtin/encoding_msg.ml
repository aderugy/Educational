(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

let rec length = function
  |[] -> 0
  |e::x -> 1 + length x;;

let reverse str =
  let rec aux = function
    |0 -> ""
    |i -> (Char.escaped str.[i - 1]) ^ aux (i - 1)
  in aux (String.length str);;

let decompose x bits =
  let rec aux = function
    |(_, 0) -> ""
    |(x, b) -> (string_of_int (x mod 2)) ^ aux ((x / 2), (b - 1))
  in reverse (aux (x, bits)) ;;

let decompose2 x =
  let rec aux = function
    |0 -> ""
    |x -> (string_of_int (x mod 2)) ^ aux (x / 2)
  in reverse (aux x) ;;

let recompose rep =
  let len = String.length rep and
  rep = reverse rep in
  let rec aux = function
    |n when n = len -> 0
    |n -> (if rep.[n] = '1' then 1 else 0) + 2 * aux (n + 1)
  in aux 0 ;;

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec bitstr = function
    |n when n = len -> ""
    |n -> (decompose (Char.code str.[n]) bits) ^ bitstr (n + 1)
  in recompose (bitstr 0);;


(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let msg = decompose2 msg in
  let msg = String.make
               ((bits - ((String.length msg) mod bits)) mod bits) '0'
            ^ msg in
  let len = String.length msg in

  let rec aux = function
    |n when n >= len / bits -> ""
    |n ->
      let rec aux2 = function
        |index when index >= bits -> ""
        |index -> Char.escaped (msg.[n * 7 + index]) ^ aux2 (index + 1)
      in (Char.escaped (Char.chr (recompose (aux2 0)))) ^ aux (n + 1)
  in aux 0;;
