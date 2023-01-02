(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
        let rec aux n =
                if (n = 0) then 1
                else x * aux (n - 1)
        in if (sign n == -1) then 1/aux (-n)
                else aux n;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let rec power x n =
        match n with
                |n when n < 0 -> power (1/x) (-n)
                |0 -> 1
                |n when modulo n 2 = 0 -> power (x * x) (n/2)
                |n -> x * (power (x*x) ((n-1)/2));;

let mod_power2 x n m =
  if x = 0 then 0
  else
    let mo = modulo x m in

    let rec aux result = function
      |0 -> modulo result m
      |n -> aux (mo * modulo result m) (n - 1)
    in aux 1 n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power base exp m =
  if base < 0 then mod_power2 base exp m else
  let rec aux result base = function
    |0 -> result
    |exp -> let r = (if ((exp land 1) > 0) then (result * base) mod m
                     else result) in
            aux r ((base * base) mod m) (exp asr 1)
  in aux 1 base exp;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  let r = n mod (p - 1) in mod_power2 x r p;;
