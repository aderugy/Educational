(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = match n with
  |1 -> false
  |n when n <= 3 -> true
  |n when n mod 2 = 0 -> false
  |n ->
    let max = (int_of_float (sqrt (float_of_int n) +. 1.)) in
    let rec aux cpt flag =
      if cpt <= max
      then flag && aux (cpt + 2) (n mod cpt <> 0)
      else flag
    in aux 3 true;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec aux = function
    | [] -> true
    | test_el::test_seq ->
       (mod_power test_el p p) = (modulo test_el p) && aux test_seq
  in aux test_seq;;
