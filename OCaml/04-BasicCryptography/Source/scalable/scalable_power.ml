(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

let sign = function
  |[] -> 0
  |e::l -> e;;

let abs = function
  |[] -> []
  |e::l -> 0::l;;

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec aux = function
    |[] |[0] -> [0;1]
    |n -> mult_b x (aux (diff_b n [0;1]))
  in aux n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let rec power x = function
  |[] |[0] -> from_int 1
  |n when sign n = 1 -> power (quot_b (from_int 1) x) (abs n)
  |n when mod_b n (from_int 2) = [] ||
            mod_b n (from_int 2) = [0] ->
    power (mult_b x x) (quot_b n (from_int 2))
  |n -> mult_b x (power (mult_b x x) (quot_b (diff_b n (from_int 1)) (from_int 2)));;

let trim l =
  let rec aux = function
    |[] -> []
    |e::l -> if e = 1 then e::l
             else aux l
  in List.rev (aux (List.rev l));;

(* & operator *)
let bwand a b =
  let rec aux = function
    |([], _) | (_, []) -> []
    |(a::lA, b::lB) -> (if a = b && a = 1 then 1 else 0)::aux (lA, lB)
  in 0::(trim (aux (a, b)));;

let shr = function
  |[] -> []
  |e::[] -> []
  |a::b::l -> a::l;;

let mod_power2 x n m =
  if x = [] || x = [0] then []
  else
    let modu = mod_b x m in

    let rec aux result = function
      |[] | [0] -> mod_b result m
      |n -> aux (mult_b modu (mod_b result m)) (diff_b n (from_int 1))
    in aux (from_int 1) n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power base exp m =
  if base <<= [] then mod_power2 base exp m else
  let rec aux result base = function
    |[] -> result
    |exp -> let r = (if (bwand exp (from_int 1) >> (from_int 1))
                     then mod_b (mult_b result base) m
                     else result) in
            aux r (mod_power2 base (from_int 2) m) (shr exp)
  in aux (from_int 1) base exp;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  let r = mod_b n (diff_b p (from_int 1)) in
  mod_power x r p;;
