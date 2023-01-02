(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(* Floor rounder square root *)
let sqrt = function
  |[] | [0] -> []
  |n when n <<! [] -> invalid_arg "negative sqrt"
  |n ->
    let rec aux = function
      |x when mult_b x x <<= n -> x
      |x -> aux (diff_b x (from_int 1))
    in aux (quot_b n (from_int 2));;

(** Deterministic primality test *)
let is_prime = function
  |n when n <<= (from_int 1) -> false
  |n when n <<= (from_int 3) -> true
  |n when mod_b n (from_int 2) = [] -> false
  |n -> let max = sqrt n in
        let rec aux cpt flag =
          if cpt <<= max
          then flag &&
                 (aux (add_b cpt (from_int 2)) ((mod_b n cpt) <> []))
          else flag
        in aux (from_int 3) true;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec aux = function
    |[] -> true
    |el::seq ->
      (mod_power el p p) = (mod_b el p) && aux seq
  in aux test_seq;;
